---
title: From Jekyll to Hakyll
read_this_to:
  - learn why and how I moved to using Hakyll for my site
i_expect_you_to_know:
  - some Haskell doesn't hurt, but it isn't really necessary
---

As you may know, I have a rather basic website with just a home page and some blog posts. I find writing blog posts in [Markdown](https://daringfireball.net/projects/markdown/) really nice. I think it is an excellent format that lets me keep focus on the actual content rather than the markup. These two things, writing in Markdown to create a simple website, come together in [Jekyll](https://jekyllrb.com/), a static site generator written in Ruby. What's more, GitHub Pages lets you push a Jekyll site source code and it will automatically transform it into the static HTML files. Pretty handy!

Setting up Jekyll for development is not fun, though. First off, you need Ruby (probably via `rvm`) and Rubygems installed, and then running `gem install jekyll` will likely end up failing multiple times because of missing native extensions. Because of [these issues](http://www.nokogiri.org/tutorials/installing_nokogiri.html), I wanted to try something different. Since I have tried learning Haskell a little, and have heard good things about [Hakyll](https://jaspervdj.be/hakyll/), that was what I reached for.

So far it's been real nice, and rebuilding the site happens almost instantaneously!


## First steps

I started by simply following the thorough [tutorials](https://jaspervdj.be/hakyll/tutorials.html). After installing [Stack](http://www.haskellstack.org/), it was just this:

```bash
$ stack install hakyll
$ hakyll-init my-site
$ cd my-site
$ stack build
$ stack exec site watch
```

And I had a demo site up and running on a development server. Sweet. Next up, I copied over my old posts from the Jekyll site and checked they showed up. They did! Of course, the templates were still the demo ones, but that was next on my list.


## Translating the templates

My Jekyll site was using Liquid templates, and Hakyll has its own little template syntax, so I had to transform them from one format to another. It really was quite simple though:

Jekyll:
```html
<title>{% if page.title %}{{ page.title }} - {% endif %}Ossi Hanhinen</title>
```

Hakyll:
```html
<title>$if(title)$ $title$ - $endif$Ossi Hanhinen</title>
```

Jekyll:
```html
{% for post in site.posts %}
  <li>
    <a href="{{ post.url }}">{{ post.title }}</a> {{ post.date | date: "%b %-d, %Y" }}
  </li>
{% endfor %}
```

Hakyll:
```html
$for(posts)$
    <li>
        <a href="$url$">$title$</a> - $date$
    </li>
$endfor$
```

If anything, the Hakyll syntax seems easier to read. More of the logic happens in Haskell code... which brings us to configuring the site. My GitHub Pages build worked in such a way that a post that was originally `_posts/2017-04-17-my-blog-post.md` ended up at the root level, without the date prefix. `ohanhi.com/my-blog-post.html`, for example. I wanted to keep this structure in order not to break anyone's links. (I also kept the `ohanhi.github.io` site up, pointing to `ohanhi.com` just in case.) By default, Hakyll uses a structure where posts are under `/posts/`, with the date prefix left untouched.

So in my `site.hs` file, I edited the posts to have a custom routing:

```haskell
-- in the `main` function
match "posts/*" $ do
    route   $ customRoute $ pathToPostName . toFilePath
    compile $ pandocCompiler
        >>= loadAndApplyTemplate "templates/post.html"    postCtx
        >>= relativizeUrls
```

I am not very good at Haskell yet, so please don't judge me, but I did this:
```haskell
pathToPostName :: String -> String
pathToPostName path =
    path
        |> splitAll "/"
        |> (\(p : name : _) -> splitExtension name )
        |> (\(n, e) -> drop 11 n ++ ".html")


(|>) = flip ($)
```

It's not the prettiest thing, but it works as long as I adhere to my filename scheme.


A trickier thing, however, was my custom lists that I like to add to the posts: `i_expect_you_to_know` and `read_this_to`. You might have seen those at the top of this post, too. In my source code they look like this:

```yaml
---
title: From Jekyll to Hakyll
read_this_to:
  - learn why and how I moved to using Hakyll for my site
i_expect_you_to_know:
  - some Haskell doesn't hurt, but it isn't really necessary
---
```

In Hakyll, the template variables `$thing$` are bound within a `Context`. So what I needed to do is somehow bring the list contents into the post's context. Luckily there was a [solution for tags](https://jaspervdj.be/hakyll/reference/src/Hakyll-Web-Tags.html) (which I don't use), so I could copy the idea from there. This is what I ended up with:

```haskell
postCtx :: Context String
postCtx =
    dateField "date" "%B %-d, %Y"
      `mappend` constField "base_url" "https://ohanhi.com"
      `mappend` listContextWith "i_expect_you_to_know"
      `mappend` listContextWith "read_this_to"
      `mappend` defaultContext
```

Works for me!

If you're interested, you can check the whole implementation from the [`site.hs` source code](https://github.com/ohanhi/ohanhi.com/blob/master/site.hs).

With that, and a couple of easy `copyFileCompiler`s, I had everything set up to build the site just like it was before.


## Deployment

Initially I thought I would keep using GitHub Pages for the hosting, but since it doesn't support building Hakyll sites on its own, I figured it would actually make my life a little harder than it could be. Essentially I could either manually run the builds and commit the built site (and not the source code) to the `master` branch, or set up a CI service to run the build and do the commits, which frankly sounds pretty daunting. I decided to look for alternatives. Zeit's `now` doesn't offer custom domains at the free tier, but [surge](http://surge.sh/) does.

Following their guide I was quickly able to get my custom domain set up and everything working. Now I can simply build the site locally and run:

```bash
~/Hobby/ohanhi.com (master)› surge _site

    Surge - surge.sh

              email: *********@gmail.com
              token: *****************
       project path: _site
               size: 34 files, 2.2 MB
             domain: ohanhi.com
             upload: [================] 100%, eta: 0.0s
   propagate on CDN: [====================] 100%
               plan: Free
              users: *********@gmail.com
         IP Address: 45.55.110.124

    Success! Project is published and running at ohanhi.com
```

Super nice!


**PS.** With this transition, I also added the Disqus comment section to all posts. Feel free to give me pointers on my Haskell on there. :)
