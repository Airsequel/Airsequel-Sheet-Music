const darkQuery = window.matchMedia("(prefers-color-scheme: dark)")

export const flags = ({ env }) => {
  const stored = window.localStorage.colorPref
  const colorPref = stored != null && stored !== ""
    ? JSON.parse(stored)
    : "auto"

  return {
    readonlyId: JSON.parse(window.localStorage.readonlyId || null),
    colorPref: colorPref,
    systemDark: darkQuery.matches
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
