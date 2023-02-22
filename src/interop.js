export const flags = ({ env }) => {
  // Called before the app starts
  return {
    readonlyId: JSON.parse(window.localStorage.readonlyId || null)
  }
}

export const onReady = ({ env, app }) => {
  // Called after the app starts
  if (app.ports && app.ports.sendToLocalStorage) {
    app.ports.sendToLocalStorage.subscribe(({ key, value }) => {
      window.localStorage[key] = JSON.stringify(value)
    })
  }
}
