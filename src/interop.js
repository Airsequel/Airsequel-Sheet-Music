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
}
