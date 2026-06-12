const darkQuery = window.matchMedia("(prefers-color-scheme: dark)")

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
