# KBC Components Index Explorer
[SPA](https://en.wikipedia.org/wiki/Single-page_application) written for educational purpose to learn and experiment with [elm lenguage](https://elm-lang.org/)(v0.19.0). It loads json from [https://connection.keboola.com/v2/storage](https://connection.keboola.com/v2/storage), parse it, optionaly filter by componentId, and render it back as json. As the json is huge, it allows to switch between simplified(default) and the whole json view.

### Development
- [Install elm ](https://guide.elm-lang.org/install.html)
- run `elm reactor`
- go to `http://localhost:8000` - opening it for the first time elm reactor will automatically install all the project dependencies(elm packages)
- go to `http://localhost:8000/src/Main.elm` and the app will start
- update source code in `src/Main.elm`

### Build
- run `./build.sh`(needs [ugilfy-js npm package](https://www.npmjs.com/package/uglify-js)) - this will create `main.min.js` in the `build` directory. Copy it along with existing `build/index.html` to destination serving it as [SPA](https://en.wikipedia.org/wiki/Single-page_application).
