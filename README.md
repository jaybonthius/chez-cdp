# chez-cdp

Drive Chromium from threaded Chez Scheme 10.4.1 over the Chrome DevTools Protocol. Chromium runs with `--remote-debugging-pipe`, so there is no port and no WebSocket: commands go to its descriptor 3 and replies and events come from descriptor 4. It needs [chez-json](https://github.com/jaybonthius/chez-json) and [chez-process](https://github.com/jaybonthius/chez-process), and Linux with `/bin/sh`.

```scheme
(import (chezscheme) (cdp) (cdp page))

(with-browser (b "/usr/bin/chromium")
  (let ([p (open-page b "https://example.com/" (page-options [width 800] [height 600]))])
    (element-text (car (page-query p "h1")))           ; "Example Domain"
    (element-style (car (page-query p "h1")) "font-weight")
    (element-click! (car (page-query p "a")))
    (page-press! p "l" (modifiers control))
    (page-screenshot p)))                               ; PNG bytes
```

## `(cdp)`: browsers, sessions, commands and events

| Form | What it does |
|---|---|
| `(browser-options [name value] ...)` | `headless` (`#t`), `profile` (a directory; default a fresh one under `TMPDIR`, deleted on close), `log` (a file for Chromium's output; default none), `arguments` (more switches), `timeout` (default deadline of every call and wait, a `time-duration`; 30 s) |
| `(launch-browser program [options])` | starts the program, which the library never searches for, and checks it answers; raises `&launch-error` |
| `(browser-close! browser)` | `Browser.close`, up to 10 s for Chromium to end, then kills its process group; deletes a profile it made; safe to call twice |
| `(call-with-browser program [options] proc)`, `(with-browser (b program [options]) body ...)` | close the browser on any exit |
| `(browser-root-session browser)`, `(browser-attach-target browser target-id)` | the browser's session, and a flat session attached to a target |
| `(cdp-call session method [params [timeout]])` | the result as an association list (chez-json's data); `params` is one too |
| `(with-events (events session method ...) body ...)`, `(call-with-events session methods proc)` | subscribes to events of the session before the body runs, until it exits |
| `(events-wait events predicate [timeout])` | the first event received that satisfies the predicate; events it passes over stay for the next wait |
| `event-method`, `event-params`, `event-session`; `browser-process`, `browser-timeout`; `session-browser`, `session-id` | accessors |

Every failure is a condition under `&error`: `&cdp-error` (`cdp-error-code`, `cdp-error-method`) when Chromium refuses a command, `&cdp-timeout` (`cdp-timeout-awaited`, `cdp-timeout-last`, the last value seen) when a deadline passes, `&browser-closed` (`browser-closed-status`, the exit status when known) when the pipe has ended, and `&launch-error` (`launch-error-program`).

One reader thread per browser reads the pipe, completes calls by id and hands each event to the subscriptions that asked for it. Different threads may use the browser and different pages at once; one page, and one events subscription, are used by one thread at a time. Chromium ends when its pipe closes, so a program that dies, even by `SIGKILL`, leaves no browser behind.

## `(cdp page)`: pages, elements and input

| Form | What it does |
|---|---|
| `(page-options [name value] ...)` | `width` (1280), `height` (800), `shared-storage` (`#f`: each page has a browser context of its own, so pages share no cookies or storage), `color-scheme` (`#f`, or `(color-scheme light)` or `(color-scheme dark)`) |
| `(open-page browser url [options])`, `(page-navigate! page url)` | return once the page's load event has fired; raise `&navigation-error` (`navigation-error-url`) when it cannot load |
| `(page-close! page)`, `page-session`, `page-browser` | close the page and its browser context |
| `(page-query page selector)` | the matching elements in document order; `'()` when none match |
| `(element-text e)`, `(element-attribute e name)`, `(element-style e property)`, `(element-box e)` | its text, an attribute or `#f`, a computed style or `#f`, its border box as a `rectangle` |
| `(page-wait-for page selector state [timeout])` | waits until an element is `(element-state attached)`, `visible` or `hidden`; returns it for the first two |
| `(page-snapshot page properties)` | a vector of `snapshot-node`s in document order, each with its name, value, parent index, attributes, bounds, layout text and the computed styles asked for, in one call |
| `(page-screenshot page [rectangle])` | a PNG bytevector of the viewport or a part of it |
| `(page-press! page key [modifiers])` | a key by its `KeyboardEvent.key` name (`"a"`, `"A"`, `"Enter"`, `"ArrowLeft"`), with `(modifiers alt control meta shift)` pressed around it |
| `(page-type! page text)` | each character as a key press; characters a US keyboard does not type are inserted as text |
| `(element-click! e)`, `(element-hover! e)` | scroll it into view and act on its centre once nothing covers it, trying until the deadline |
| `(page-wheel! page point dx dy)`, `(page-drag! page from to)` | with `(make-point x y)` in CSS pixels |

Reads use the protocol's DOM, CSS and DOMSnapshot domains and run no JavaScript in the page. An element is its selector and its index among the matches, looked up again on every use, so an element the page replaces with an equal one is still found; one that is gone raises `&element-not-found`. Input arrives as trusted events, with a US keyboard's codes; each modifier is pressed as its own key.

## Building and testing

```sh
make SCHEME=/path/to/chez-10.4.1/scheme deps build
make SCHEME=/path/to/chez-10.4.1/scheme CHROMIUM=/path/to/chromium test
```

The libraries are found beside this one or at `JSON_DIR`, `PROCESS_DIR` and `TEST_DIR` ([chez-test](https://github.com/jaybonthius/chez-test), for the tests). Programs import `(cdp)` with `--libdirs /path/to/chez-cdp/src::/path/to/chez-cdp/build/obj/src` and the other two libraries' directories. `CAP` prefixes every build and test process. Tested with Chromium 146.0.7680.177 on Linux. Chromium dies at start when `TMPDIR` is so long that a socket path inside it does not fit.
