---
title: My Open Source Journey
author: Jan van Brügge
date: 2020-10-28
tags: [open-source]
description: It's October again, Hacktoberfest time. Also time for me to reminisce about my personal learning journey that was so heavily influenced by open source.
---

*This post was first published on the [Futurice Blog](https://futurice.com/blog/my-open-source-journey).*

It's October again, Hacktoberfest time. Also time for me to reminisce about my personal learning journey that was so heavily influenced by open source.

## The Beginning

My interest in computers and coding started rather early, so when I was 14 years old, I went to the local library, got myself a book about programming ("From Zero to Hero: Java") and started on this wonderful path that is now not only my job, but also my passion.

I first came in contact with open source - well other than using excellent software like VLC - was when I encountered a problem with my music library. My dad and I had built a NAS server that I put Linux on to serve our media with a Plex Media server. We also used iTunes to sync parts of the library to mine and my sister's iPod touch. But many of the songs and especially the audiobooks had the wrong metadata, so iTunes and thus the iPod did not sort them correctly! Plex however used the filenames and based on those downloaded the correct metadata from the internet. So the question was: Is it possible to have Plex write the correct metadata for the files so iTunes would also have them?

After a bit of searching the internet I found [Plex Media Tagger](https://github.com/ccjensen/PlexMediaTagger) which did exactly the thing I wanted! Sadly however it used "subler" for writing the metadata, a software that only works on MacOS and not on our Linux NAS. With the loads of time on my hands that comes with being a high school student, I wanted to try and make it work also on Linux. The tagger is written in Python, but at that point I had already tried my hand in several other languages like C++ and JavaScript, so I was confident that I just needed to take a look at the syntax - programming languages are all the same, right? (A misconception I would realise much later).

With this new goal in mind, I again searched around and found a pure python library that could write the metadata, so I could replace subler and have a pure python plugin that could run on any operating system! I forked and cloned the repository (I already used git for my personal code) and got the changes done rather quickly. It worked well enough on the Linux NAS, so I shared my work in order for others to benefit from it. Knowing not much about programming and good code style, I basically just copy pasted the python library into the project and commited that. You can see the horror that is the PR [here](https://github.com/ccjensen/PlexMediaTagger/pull/9). Apparently I missed some stuff or there are differences in the metadata for different operating systems, so it never got merged (I also did not really care about it as it was working well enough for myself).

## Getting Involved More Consistently

I however would mark the start of my journey a bit later. It was near the end of high school and I had started freelancing on a project for a client of my dad. This was the first big web project I had. Before, I only played around with a bit of PHP and jQuery, but that had more of a duct tape feel than a properly organized application that I would be able to maintain for more than a month. So I started looking around what the status quo was for frontend development. At that time Angular 2 was in alpha and I was amazed. Two way data binding! A proper way to structure your app into components and more. It also introduced me to RxJS which I found intriguing. But it had a lot of issues. My two main gripes were debuggability - you would get a stack trace that mostly consisted of Angular code and was not really helpful - and I did not understand RxJS correctly, so I was burned by some hot/cold stream issues. To fix those I found ["The introduction to Reactive Programming you've been missing"](https://gist.github.com/staltz/868e7e9bc2a7b8c1f754), which cleared a lot of the misconceptions I had. Sadly in the end, the debugging experience ate my productivity so I searched for something to replace Angular.

The two pieces I wanted to keep from Angular were RxJS and Typescript. I had always coded in typed languages (Java and C++ mainly), and I really hated that JavaScript did nothing to prevent stupid errors on my side. I've seen "undefined is not a function" too many times even in those short tries with jQuery. Again searching around for frontend frameworks, I came across React, and saw a conference talk on YouTube about the virtual DOM that React uses. I liked the idea, but found the execution lacking.

In the end I stumbled across [Cycle.js](https://cycle.js.org/) that was created by André Staltz, the same person who wrote the reactive programming introduction that basically went all in on RxJS by centering the whole framework around it. It was also recently converted into TypeScript at that time, so that was another plus.

## The Normal Open Source Trajectory

From now on I was on what I would call a typical open source trajectory. I used the Cycle.js framework to rewrite my frontend and in that process I hit some walls. I eventually figured that the error was on my side and that I was just missing some information to avoid the error. To spare others the hours of debugging I started to [contribute small patches](https://github.com/cyclejs/cyclejs/pulls?q=is%3Apr+author%3Ajvanbruegge+sort%3Acreated-asc+) to the documentation. At the same time I also found some missing features that I voiced in GitHub [issues](https://github.com/cyclejs/cyclejs/issues/441).

All in all I got more involved in the community and soon I also started fixing bugs myself that I found, instead of just opening issues. I got more known by the core team. This was then the reason I was asked if I want to speak at [CycleConf 2017](https://futurice.com/blog/cycleconf-2017-attracted-some-very-different-cyclists-to-stockholm-this-spring) in Stockholm. This being my first conference I was quite excited and in the end it was a fantastic experience that I still remember fondly. Shortly after, I was invited to become part of the core team myself, getting access to the [OpenCollective Funds](https://opencollective.com/cyclejs) and helping steer the framework together with André and Nick.

## Continuing My Learning

During that time I also wanted to dive deeper into functional programming. When I first came in contact with RxJS, it was the first time I learned about higher order functions like `map` and `reduce` and I really liked the concept. It allowed me to express _what_ I wanted to do, not really caring about _how_ exactly it was done. So after hearing the [fantastic talk about Ramda](https://www.youtube.com/watch?v=m3svKOdZijA), I was addicted. So I again searched around for functional languages because I thought that this way I could really learn the concepts in their purest form and it would prevent me from falling back to my old patterns - mainly OOP. This turned out to be one of the better decisions I made in my life. I started to learn Haskell, at the beginning with [Learn you a Haskell for Great Good](http://learnyouahaskell.com/chapters) and later with [Real world Haskell](http://book.realworldhaskell.org/read/) which is sadly a bit outdated (so do not expect to follow the code 100%), but the general ideas still hold true today.

This was the first time since starting to program that a new way of thinking about code and algorithms was opened to me. Through me learning Java and C++ as my first languages, I used to solely think in for-loops and if-conditions. But being forced to write purely functional code makes you think differently about problems. At least for me, it pushed me to think more about the data structures and not only about the algorithms. It showed me that there are more ways to do concurrency than slapping a `synchronized` keyword everywhere. That mutable state is the root of all evil and just so much more.

## My Academic Path

Learning Haskell also set me on a more research-focused path. I had already started my computer science studies, but after learning Haskell I became more interested in the theoretical science. I wanted to see _why_ languages are like they are. What are the similarities? What are the pros and cons of e.g. garbage collection. And especially type systems became really intriguing to me. Haskell is just so much more expressive than for example Java where the type system can be a burden at times. While not perfect, in Haskell the type system is much more helpful, preventing you from making dumb mistakes, and with some more work on your side, even not so dumb mistakes.

I got more into theorem proving, i.e. using a programming language to write mathematical proofs and have them checked by a compiler. Step by step I am working on a [machine checked proof of type safety of System Fc](https://github.com/jvanbruegge/isabelle-lambda-calculus), the intermediate language of the Haskell compiler.

## In Retrospect

Looking back at my journey, there were a few key events that brought me to where I am now. The first one was getting this freelance project that made me look more into modern frontend frameworks. Then, through Angular, getting to know RxJS and thus getting in contact with functional programming. Going deeper into Cycle.js and being invited to speak at my first conference. And lastly starting to learn Haskell which set me on a more research oriented path.

I am also super grateful that the Open Source work I do is sustainable for me. This is in part thanks to our generous backers on [OpenCollective](https://opencollective.com/cyclejs), but also through Futurice, the company I work for. Futurice has the [Spice Program](https://spiceprogram.org/) that pays a flat hourly rate for any Open Source work any employee does in their free time. Those two sources of income would not be enough to work full time, but they allow me to have dedicated days that I can focus on Cycle.js.

All in all, I can say that every time I learned something fundamentally different and new, even if I would not use it in my day to day job, I've really benefited from the experience. So in case you want advice from a 23 year old, keep learning! Try out completely new stuff even if it is not immediately useful for you! My style of writing code even in imperative languages has evolved a lot over the years.
