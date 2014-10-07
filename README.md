About Purplish
--------------
Purplish is an imageboard software for [Radiance](https://github.com/Shinmera/Radiance). It supports multiple boards, themes, multiple files per post, rate limiting, video and audio embedding, post editing and more.

How To
------
Set up Radiance and load Purplish with quicklisp or ASDF. Once Radiance has been started up, you can use `purplish:create-board` to add new boards. Purplish occupies the subdomain `chan`. You can set up the rotating headers by putting images into `purplish:*headers*`.

Interface Dependencies
----------------------
* database
* data-model
* auth
* cache
* profile
* rate

Configuration Variables
-----------------------
* `(:purplish :title)`
* `(:purplish :description)`
* `(:purplish :thumb :width)`
* `(:purplish :thumb :height)`
* `(:purplish :thumb :gif)`
* `(:purplish :file :size-limit)`
