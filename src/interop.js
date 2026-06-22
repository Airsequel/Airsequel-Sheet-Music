import * as pdfjsLib from "pdfjs-dist"
import PdfWorker from "pdfjs-dist/build/pdf.worker.min.mjs?worker"

// Let Vite bundle the worker and hand pdf.js a ready Worker instance.
// This works in both dev and production, unlike pointing workerSrc at a
// bare ESM URL (which fails to instantiate in dev). Only one PDF is open
// at a time here, so a single shared worker port is sufficient.
pdfjsLib.GlobalWorkerOptions.workerPort = new PdfWorker()

const darkQuery = window.matchMedia("(prefers-color-scheme: dark)")

// PDFs are loaded once per URL and shared between the page-count lookup
// and the per-page <pdf-page> elements, so the document is only fetched
// and parsed a single time.
const pdfDocCache = new Map()
function loadPdf (url) {
  if (!pdfDocCache.has(url)) {
    pdfDocCache.set(url, pdfjsLib.getDocument({ url }).promise)
  }
  return pdfDocCache.get(url)
}

// Loads a PDF only to report its page count back to Elm via a "numpages"
// event, so Elm can then mount one <pdf-page> per page. Renders nothing.
if (!customElements.get("pdf-doc")) {
  customElements.define("pdf-doc", class extends HTMLElement {
    static get observedAttributes () {
      return ["url"]
    }

    connectedCallback () {
      this.load()
    }

    attributeChangedCallback () {
      this.load()
    }

    async load () {
      const url = this.getAttribute("url")
      if (!url || this._loadedUrl === url) return
      this._loadedUrl = url
      let numPages = 0
      try {
        const pdf = await loadPdf(url)
        numPages = pdf.numPages
      }
      catch (error) {
        this._loadedUrl = null
        numPages = 0
        console.error("pdf-doc: failed to load", url, error)
      }
      this.dispatchEvent(
        new CustomEvent("numpages", { detail: { numPages } })
      )
    }
  })
}

// Render a PDF page to a <canvas> so it can be displayed exactly like an
// image page (same page numbers, dividers, color schemes and width
// controls applied by the surrounding Elm layout). One element renders
// one page; Elm mounts as many as the document has pages.
if (!customElements.get("pdf-page")) {
  customElements.define("pdf-page", class extends HTMLElement {
    static get observedAttributes () {
      return ["url", "page", "direction", "max-width", "multipage"]
    }

    connectedCallback () {
      this.render()
    }

    attributeChangedCallback (name) {
      // Re-styling is cheap, but re-rendering the page is not, so only
      // the layout-affecting attributes restyle without a full re-render.
      if (name === "direction" || name === "max-width"
          || name === "multipage") {
        this.styleCanvas()
      }
      else {
        this.render()
      }
    }

    styleCanvas () {
      if (!this.canvas) return
      const direction = this.getAttribute("direction") || "vertical"
      const maxWidth = this.getAttribute("max-width")
      const multipage = this.getAttribute("multipage") === "true"
      const style = this.canvas.style
      style.display = "block"
      // object-fit: contain guarantees the page's aspect ratio is never
      // changed, even if the flex layout stretches the canvas box.
      style.objectFit = "contain"
      if (direction === "horizontal") {
        style.alignSelf = "auto"
        if (multipage) {
          // Mirror the horizontal multi-page <img>: cap the width, fill
          // the height.
          style.maxWidth = maxWidth ? maxWidth + "rem" : "none"
          style.maxHeight = "none"
          style.height = "100%"
          style.width = "auto"
        }
        else {
          // Mirror the single-page <img>: fit within the viewport.
          style.maxWidth = "100%"
          style.maxHeight = "100%"
          style.height = "auto"
          style.width = "auto"
        }
      }
      else {
        // Mirror the vertical <img>: full width, capped at max-w-6xl.
        style.alignSelf = "center"
        style.width = "100%"
        style.height = "auto"
        style.maxWidth = "72rem"
        style.maxHeight = "none"
      }
    }

    async render () {
      const url = this.getAttribute("url")
      const pageNum = Number.parseInt(this.getAttribute("page"), 10)
      if (!url || !Number.isFinite(pageNum)) return

      const token = url + "#" + pageNum
      if (this._renderedToken === token) return
      this._renderedToken = token

      try {
        const pdf = await loadPdf(url)
        const page = await pdf.getPage(pageNum)
        // Render above CSS size so the page stays crisp when zoomed in,
        // but cap the longest side so multi-page PDFs don't exhaust memory.
        const base = page.getViewport({ scale: 1 })
        const maxSide = 2400
        const scale = Math.min(
          2 * (window.devicePixelRatio || 1),
          maxSide / Math.max(base.width, base.height)
        )
        const viewport = page.getViewport({ scale })
        const canvas = document.createElement("canvas")
        canvas.width = viewport.width
        canvas.height = viewport.height
        await page.render({
          canvasContext: canvas.getContext("2d"),
          viewport
        }).promise
        this.canvas = canvas
        this.styleCanvas()
        this.replaceChildren(canvas)
      }
      catch (error) {
        this._renderedToken = null
        console.error("pdf-page: failed to render", url, pageNum, error)
        const message = document.createElement("p")
        message.textContent = "Could not render PDF page"
        message.style.fontFamily = "sans-serif"
        message.style.padding = "1rem"
        this.replaceChildren(message)
      }
    }
  })
}

// Metronome as a custom element: Elm mounts it to start ticking and
// unmounts it to stop, so playback can never outlive the play view.
// Clicks are scheduled ahead of time via the Web Audio API because
// JS timers alone are too imprecise for a steady beat.
if (!customElements.get("metronome-player")) {
  customElements.define("metronome-player", class extends HTMLElement {
    get bpm () {
      const value = Number.parseInt(this.getAttribute("bpm"), 10)
      return Number.isFinite(value) && value > 0 ? value : 90
    }

    connectedCallback () {
      this.audioCtx = new AudioContext()
      // The element is mounted as a result of a click,
      // so resuming is permitted by autoplay policies
      this.audioCtx.resume()
      this.nextTickTime = this.audioCtx.currentTime + 0.1
      this.timer = setInterval(() => this.scheduleTicks(), 25)
    }

    disconnectedCallback () {
      clearInterval(this.timer)
      this.audioCtx.close()
    }

    scheduleTicks () {
      const lookahead = 0.1 // seconds
      while (this.nextTickTime < this.audioCtx.currentTime + lookahead) {
        this.click(this.nextTickTime)
        this.nextTickTime += 60 / this.bpm
      }
    }

    click (time) {
      const oscillator = this.audioCtx.createOscillator()
      const gain = this.audioCtx.createGain()
      oscillator.frequency.value = 1000
      gain.gain.setValueAtTime(1, time)
      gain.gain.exponentialRampToValueAtTime(0.001, time + 0.05)
      oscillator.connect(gain)
      gain.connect(this.audioCtx.destination)
      oscillator.start(time)
      oscillator.stop(time + 0.05)
    }
  })
}

export const flags = ({ env }) => {
  const stored = window.localStorage.colorPref
  const colorPref = stored != null && stored !== ""
    ? JSON.parse(stored)
    : "auto"

  let horizontalSongSettings = {}
  try {
    horizontalSongSettings = JSON.parse(window.localStorage.horizontalSongSettings || "{}")
  }
  catch (error) {
    // Corrupt data must not break startup; Elm falls back to defaults
  }

  return {
    readonlyId: JSON.parse(window.localStorage.readonlyId || null),
    colorPref: colorPref,
    systemDark: darkQuery.matches,
    horizontalSongSettings: horizontalSongSettings
  }
}

export const onReady = ({ env, app }) => {
  if (app.ports && app.ports.sendToLocalStorage) {
    app.ports.sendToLocalStorage.subscribe(({ key, value }) => {
      window.localStorage[key] = JSON.stringify(value)
    })
  }

  if (app.ports && app.ports.systemDarkChanged) {
    const notify = event => app.ports.systemDarkChanged.send(event.matches)
    if (darkQuery.addEventListener) {
      darkQuery.addEventListener("change", notify)
    }
    else if (darkQuery.addListener) {
      darkQuery.addListener(notify)
    }
  }

  // Translate vertical wheel input into horizontal scroll
  // for containers marked with data-scroll-direction="horizontal".
  document.addEventListener("wheel", (event) => {
    let element = event.target
    while (element && element !== document.body) {
      if (element.dataset
          && element.dataset.scrollDirection === "horizontal") {
        if (Math.abs(event.deltaY) > Math.abs(event.deltaX)) {
          event.preventDefault()
          element.scrollLeft += event.deltaY
        }
        return
      }
      element = element.parentElement
    }
  }, { passive: false })
}
