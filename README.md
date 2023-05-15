# Orgish

Org is HTML. (or) Org in and as HTML.

## What?

Ok, let me explain a bit. What if a single file is both an org file and its exported html output? The benefits of 2 formats in a single form factor:

1. Open the file in emacs: it is in Org mode! Edit it and simply save it.
2. Open the same file in the browser: it is an html file! It has the updates you just did.

## Why?

I have a few static html pages on my website that are small, single-purpose and updated only rarely. Every single time I try to update these, I regret not having the source in org format.

For many usecases, exporting from org is a great solution - I use it in various ways including my blog. But, for these single stand alone pages, it is not that seamless: you now have 2 files to worry about. While the html side is straight-forward - you just stick it on a static hosting server, you have to manage your org source somewhere. **By chance, if you lose the org source, you are done for**.

Point is: there are a few situations where having a source and a generated file is basically just un-needed overhead.

## How?

Very simple, nothing fancy. We manage the entire org source as a comment with a special "org" header/footer as the first line of the html file.

The file is a normal html file for the rest of the world. Nothing out of the ordinary - as far as browsers and other users are concerned. No one, but you, knows that the file is actually **orgish**.

In Emacs, with a little bit of elisp magic, when you try to open the file, if it is *orgish*, we extract out the source and present it as an org buffer. When you save this buffer, we run the normal org-mode html export and then prepend the source org file as a comment.

## Usuage

```
git clone https://github.com/ChanderG/orgish
(load-file "<path to orgish>/orgish.el")
```
This adds a hook to html mode, so existing orgish files will be opened automatically. To create new files, use `orgish/open`.

## Other stuff

### Is there an example?

The only real example so far, from my website: https://www.chandergovind.org/publications.html
I am looking to create all new stand-alone pages on my website using orgish.

### Can I export to some other format?

When you open the file in Emacs with orgish present, you are given an Org-mode buffer. You can do whatever you want there - including exporting to other formats. The only thing orgish gives you out of the box (apart from the encoding/decoding) is that it exports to html automatically without you having to do anything.

### Isn't this inefficient?

Yes. I briefly considered fancy encoding schemes since every string in the org source will appear in the html file below it. I discarded these options, for two reasons:

1. These are small stand-alone pages. There is no dearth of storage and bandwidth for such small pages to begin with - a single image will outsize your entire html page. Also, let us not forget, as great man once said: premature optimization is the root of all evil.
2. Simplicity helps with longevity: by chance, 100 years from now, someone finds an orgish page in the wild: they can simply copy out the org source and continue. Html, emacs and org-mode will all make it to the next century, but this package most likely will not.

### This isn't secure!

Yes. We encode the entire org plaintext into the final output. This means, even unexported stuff from your org file will be present, albeit hidden, in the html file.

One option is to encrypt the source file (have opened an issue, but will not work on it unless someone really needs this). But note: if we go down this route, it will become very easy to lose your work by losing your encryption password. It is probably a better idea to simply use org normally and export to html as needed.

## License

MIT
