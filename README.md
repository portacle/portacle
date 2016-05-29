## About Portacle
Portacle is a PORTAble Common Lisp Environment. What this means is that it is a fully featured integrated development environment for use with Common Lisp that runs on all major platforms and can even be put onto a USB stick to carry around.

## Why?
One of the major annoyances when coming to lisp is that you have to go through a rather involved procedure to get everything set up and acquire a useful development environment. This may not be that much of a hassle for seasoned veterans or people experienced with programming and system management already, but for absolute beginners it is a long sequence of confusing and error-prone steps to take.

Aside from this, I've often wanted to be able to put my environment onto a USB stick so that I can work on things at any particular machine without having to set it up every time.

## Using It
In order to launch Portacle, simply run one of the files appropriate for your system from the root directory. Namely `portacle.exe` for Windows, `portacle.desktop` for Linux, and `Portacle.app` for OS X.

After a bit you should be presented with a window like this:

![portacle-window](https://filebox.tymoon.eu/file/TVRBMU9BPT0=)

It might spit out some messages and notes before it looks like that. You can safely ignore those.

![portacle-repl](https://filebox.tymoon.eu/file/TVRBMU5nPT0=)

An extensive explanation of the various keychords and all that would follow here if I had more time at the moment.

## Updating It
In order to update all of the configuration files and packages, simply hit `M-x portacle-update RET`.

However, this will not update emacs, sbcl, and git themselves, as they need to be compiled to reach a newer version. Either see if there is a [new release](https://github.com/Shinmera/portacle/releases/) or if you can build it yourself if needed. Fortunately you should not have to do this very often, if ever at all. The above `portacle-update` command should suffice for most scenarios.

## Building It
In order to build the portacle environment yourself, please refer to the [README](https://github.com/Shinmera/portacle/tree/master/build) in the build/ directory of the repository.
