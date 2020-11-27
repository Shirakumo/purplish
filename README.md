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
* `(:title)`
* `(:description)`
* `(:thumb :width)`
* `(:thumb :height)`
* `(:thumb :gif)`
* `(:file :size-limit)`

Permissions
-----------
* `(purplish post create)`
* `(purplish thread create)`
* `(purplish post change)`
* `(purplish thread delete)`
* `(purplish post purge)`
* `(purplish board create)`
* `(purplish board delete)`
* `(purplish post move)`
* `(purplish thread move)`
* `(purplish admin cache)`
* `(purplish admin config)`
* `(purplish admin headers)`
* `(purplish admin themes)`
* `(purplish admin boards)`
