# ohanhi.com

This is the source code for my website, [ohanhi.com](https://ohanhi.com/).

## How it works

This is a [Hakyll](https://jaspervdj.be/hakyll/) site. In general, the structure is like this:

```
├── css
│   └── main.css
├── img
│   └── ...
├── js
│   └── ...
├── posts
│   ├── 2015-06-10-learning-fp.md
│   └── ...
├── templates
│   ├── default.html
│   ├── post.html
│   └── ...
├── ...
├── CNAME
├── index.html
├── site.hs
└── stack.yaml
```

First off, `CNAME` has my custom domain name in it, so you will likely want to remove the file or replace its contents.

If you want to take a look at the Haskell code, the main configuration is in `site.hs`. The HTML template code is mainly in `templates/`, but `index.html` is at the root level.

The blog posts themselves are Markdown files in the `posts/` directory, and their filenames have a very specific pattern. They get turned into HTML files like this:

```
/posts/2015-06-10-learning-fp.md            ->  /learning-fp.html
/posts/2016-02-18-phoenix-ssl-localhost.md  ->  /phoenix-ssl-localhost.html
```

My configuration for this is not too sophisticated, and so the directory depth of 1 and the date in the filename are essential.

## Building

Assuming you have [Stack](https://haskellstack.org/) installed, you can:

```bash
# build the site configuration
$ stack build
# generate the static site
$ stack exec site build
# also clean the files in between
$ stack exec site rebuild
```

## Deployment

Netlify auto-deploys from the `built-site` branch, so I have a shell script to do all the steps needed to build and deploy from the CLI.

```bash
$ bash deploy.sh
```

## License

Source code licensed under [BSD (3-clause)](LICENSE).

Content licensed under [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/).
