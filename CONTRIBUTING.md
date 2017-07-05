# Information for Potential Contributors

## How to Be Helpful
This is a project whose primary ache is the platform dependence. Every platform, and every different version of a platform, comes with its own set of problems and unique traits that may make the system break. You can thus be most helpful by testing thoroughly and reporting the issues you encounter in detail and as clearly as possible.

Specifically, if you encounter an issue you should:

* State clearly what you did to arrive at the problem
* What operating system you are on, and which version it is at
* Supply all possible output or information that describes the problem

While [filing an issue](https://github.com/Shinmera/portacle/issues) is a great way to help out in the case of real bugs in the software, if you're just confused about something and aren't sure whether it is or isn't a bug, you can also try asking for help on IRC: irc.freenode.org/#shirakumo

## Policy on New Components
Portacle is intended to provide a somewhat minimal, but still complete enough development environment for novices and average users. Thus, if you have an additional component you would like to add to Portacle, consider the following points.

* Is it something that would be useful to most people, including novices?
* Can it be built from source and does it work on Linux, OS X, and Windows?
* Is it still actively maintained today and won't be thrown away before long?
* Can you configure where it looks for user/data files and where it stores temporary/data files?
* Is it relocatable? If you move the component's directories around, will it still work?

If the answer to any of those questions is "No," it is virtually guaranteed that I will reject any requests to add the component, even if you do all the legwork of integrating it into the build system.

## Accompanying Documentation
Portacle includes a lengthy [help file](https://github.com/Shinmera/portacle/blob/master/config/help.txt), which is supposed to give a primer on how to use it and the general concepts of the Emacs IDE. I believe that documentation of this kind can always use improvements and additions, so I welcome any and all to correct, clarify, and extend this document to be more useful to beginners.

## How to Develop Portacle
The development of Portacle consists primarily of the maintenance of a couple of shell scripts in the [build/](build/) directory. These scripts are responsible for automatically assembling a full Portacle distribution that is ready for shipment. However, it is also plausible that they, too, are fallible in some form, so testing of the build process is also appreciated.

Aside from the build scripts, Portacle also includes a launcher application that is responsible for preparing the necessary environment and starting up the various applications that are shipped in the bundle. For portability reasons, the launcher is written in C. If you intend on developing it, or want to add a new component to Portacle, see [portacle-launcher](https://github.com/Shinmera/portacle-launcher).

Finally, Portacle includes a set of elisp configuration files that are responsible for creating a more enjoyable Emacs setup that works within the Portacle distribution. These files are located in the [portacle-config](https://github.com/Shinmera/portacle-config) repository.

Before starting to work on things, you should first try to build Portacle on your system. See the readme in the build directory for instructions on how to get started. It is highly recommended that you do builds within a clean virtual machine as to avoid accidentally developing Portacle in a way that is too specific to your system.

### Short Shell Style Guide
Everything should be placed within a function, or the predefined build function stubs. The only thing that should be at the top level of a script are variable declarations, the loading of the `common.sh`, and the running of the `main` function at the very end.

Use `local` for function-local variables wherever possible and name them in lower case. Variables at the top level should be `readonly` if possible, and in all caps.

Try to fail and exit as early as possible. Use `|| eexit "Oh no"` or similar whenever a potential failure might occur to break out of the function and abort the build. Not exiting early enough will just produce confusing errors down the line and make debugging harder.

Use the functions provided in `common.sh` wherever possible, as they have already been tested for platform compatibility and will likely fill most of your needs.
