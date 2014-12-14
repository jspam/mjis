MiniJava in Scala [<img src="https://api.travis-ci.org/MiniJavaInScala/mjis.png"/>](https://travis-ci.org/MiniJavaInScala/mjis)
=================

How to get started
------------------

 - Install the [JDK for your platform](   http://www.oracle.com/technetwork/java/javase/downloads/jdk8-downloads-2133151.html)
 - Install [SBT](http://www.scala-sbt.org/0.13/tutorial/Manual-Installation.html)
 - Done.

Creating a stand-alone binary
-----------------------------

 - Run `sbt binary` from inside the mjis directory
 - Location of compiler: target/scala-2.11/libfirm.so
 - Use the compiler, e. g. `./libfirm.so Hello.java`

Generating IDE project files
----------------------------

The SBT plugins for Eclipse and IntelliJ are already pre-installed.
You could also create a new project from your IDE, but trust me, you don't want that (looking at you, Eclipse).

Therefore, after you have cloned your repo:
 - Go into the directory: `cd mjis`
 - Run SBT from that directory to either generate Eclipse or IntelliJ project files:
   `sbt eclipse` or `sbt gen-idea`

Now you can import the mjis project in your IDE.

Contributing
------------

 - Fork https://github.com/MiniJavaInScala/mjis
   (`mjis` is the repository, `MiniJavaInScala` is the organization)
 - Clone your fork: `git clone git@github.com:<your-username>/mjis.git`
 - Add the organization's repository as a remote repo:
   `git remote add trunk https://github.com/MiniJavaInScala/mjis`

A few notes, as discussed:

 - Work on your local fork, never on the organization's repository itself.
 - Protip: Use branches for your work, not master.
 - Push to your fork on GitHub, not to the organization's repository directly.
 - After pushing to your fork on GitHub, you can create a pull request from the website.
   Example: https://github.com/MiniJavaInScala/mjis/pull/2
 - Pick a "random" person to review and merge your changes.
   If everything is ok, a "LGTM" (Looks good to me) is perfectly fine answer.
   Example: https://github.com/MiniJavaInScala/mjis/pull/2
 - If your work fixes an issue from our bugtracker, mention the issue in your commit message.
   Example: https://github.com/MiniJavaInScala/mjis/pull/2
 - How to format Git commit messages: http://tbaggery.com/2008/04/19/a-note-about-git-commit-messages.html
