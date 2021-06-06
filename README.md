`ephemeral` dredges up highlighted words from e-books for review.

#### Usage

- `ephemeral` writes to the directory in which it is executed. Sorry. I need to implement a configuration file.
- Use with a `crontab -e` entry like `0 8 * * * cd /path-to-executable && ./ephemeral --select`. `ephemeral` produces a file called `word.html`. [2]

#### Supported e-books

- Amazon's Kindle, with `--kindle-import`. [1]

![An HTML file produced by ephemeral showing the word "chicanery".](https://www.dropbox.com/s/zm1zfx41aigrhs8/word.png?raw=1)

[1] Until they break my CSV parser. Please make a Kindle API, Amazon.

[2] I've bookmarked it in my browser's bookmark bar. Unfortunately there's no way to interact with the program from this page. I'd like to implement a web server in the future.
