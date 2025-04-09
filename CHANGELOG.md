# Change Log

All notable changes to this project will be documented in this file.

Check [Keep a Changelog](http://keepachangelog.com/) for recommendations on how to structure this file.


## 1.9.0 (Unreleased)
> Released N/A

* fix: Make remove entry optional (#480)
* Add dashboard icon face (#483)
* Add customization option to hide the cursor on the dashboard (#492)
* Add option to vertically center dashboard (#318 and #493)
* fix: Adding animation to webp image (#502)
* feat: Support for insert startupify list in order (#506)
* Support for random banners (#510)
* Support for random footer icon (#519)
* feat: Parameters for file icon `height` and `v-adjust` (#546)
* fix: Show `installed packages` instead of `loaded packages`. (#570)

## 1.8.0
> Released Jul 26, 2023

* Disable display-line-numbers-mode (#182)
* Mute no project removed message (#212)
* Align the file icons horizontally (#221)
* Fix package count when using package.el and straight.el simultaneously (#233)
* Fix fitler agenda by time (#232)
* Respect to custom items face instead of widget-button face (#273)
* Support for image transforms (#278)
* Allow moving point during `dashboard-mode-hook` (#227)
* Add truncate style for path (#279)
* Add agenda highlight headlines (#281)
* Fix mosue click error (#285)
* Add flag to refresh instead to kill the dashboard buffer (#290)
* Fix wrong recent files when first starting the dashboard buffer (#294)
* Add license file (#296)
* Fix icon for week agenda items (#300)
* Make `page-break-lines` as optional dependency (#315)
* Add feature to customize section headings (#316)
* Add gif support (#327)
* Fix init info reporting with wrong time (#330)
* Add new ASCII GNU Emacs logo, `banners/4.txt`. (#337)
* Add feature to sort agenda (#335)
* Update the CI process uasing Cask (#341)
* Add a changelog (#342)
* Make shortcut function name respect to shortcut id (#348)
* Add capability to custom align agenda widget (#350)
* Correct straight.el package count (#354)
* Lazy load modules specify in dashboard buffer (#359)
* Clean up zombie project for project.el (#363)
* Add capability to navigate each section (#365)
* Add functionality to delete items (#366)
* Fix issue dashboard-banners-directory resolves with two slashes ("dashboard//banners") (GNU only?) (#367)
* Add capability to remove item for agenda (#372)
* Add CI, activation test (#381)
* Fix dashboard not showing up in daemon mode (#382)
* Calculate truncate path length in pixel (#402)
* Center banner with properties and combine text with image (#407)
* Caculate line length in pixel width (#427)
* Add ascii option to dashboard-startup-banner (#436)
* Agenda tags format (#441)
* Make icon display predicate customizeable (#442)
* Add support for nerd-icons (#451)
* Add custom variables for icon's arguments (#461)

## 1.7.0
> Released Feb 21, 2020

* Update year in Copyright
* Fix CI and include emacs 27 (#218)
* Disable package-lint checks for now
* Fix error when `dashboard--section-starts' is nil (#204)
* Smaller icon size for remote files (#211)
* Create a custom variable for dashboard-footer (#186)
* Replace defvar with defcustom, making customize work properly (#205)
* Disable undo history for dashboard buffer (#207)
* Avoid loading `all-the-icons` prematurely (#209)
* Get elisp-lint path using find
* Refactor: use all-the-icons-icon-for-dir (#198)
* Update Emacs 26 to 26.3
* Update elisp-lint package
* Update circe to 2.1
* Replace if with when (#200)
* Fix space after comma for phrase in dashboard-footer (#194)
* Fix face description typo (#189)
* Fix widget face bug. Fixed some linting issues (#177)
* Dashboard return button (#171)
* Fix icons for weekly agenda and add an icon final condition (#173)
* Added filtering by category for org mode (#167)
* Revert "Remove `(require 'seq)` (#165)"
* Remove `(require 'seq)` (#165)
* Restore seq-take properly
* Replace `seq-take` with its code (#161)
* Reduce initial requires (#162)
* Update screenshot
* Vimish bindings (#139)

## 1.6.0
> Released Jun 07, 2019

* Added foot note plus several enhancements and code improvements (#137) @romatthe
* Do not require all-the-icons for footer icon, use it only if it is available (#141) @JesusMtnez
* Properly use footer icon fallback text (#142) @JesusMtnez
* Add face: dashboard-footer (#145) @seagle0128
* Clean up project list before loading it (#144) @seagle0128
* Add the navigator below the banner with the customized buttons. (#143) @seagle0128
* Support `straight.el` properly in `dashboard-init-info` (#147) @JesusMtnez
* Support multiple lines for the navigator. (#150) @JesusMtnez
* Add newline at the end of insert footer. (#149) @jcs090218
* Update metadata in comments @JesusMtnez
* Drop 24.4 support, from now on checking from emacs 25.3 @JesusMtnez

## 1.5.0
> Released May 27, 2019

* Support bookmark/agenda/register/project actions. (#131)
* Made heading icons a customizable variable (#135)

## 1.4.0
> Released May 22, 2019

* Update emacs versions used in CircleCI (#124)
  - Emacs 25.3
  - Emacs 26.2
  - Emacs master
*  Added heading and file icons (#123)
*  Added new init info functionality (#126)

## 1.3.1
> Released Mar 20, 2019

* No need to launch 'projectile-mode (#113)
* Use "Applications" group and give a proper description (#114)
* Fix error in `dashboard-next-section` (#118)
* Make line moving commands behave like `dired` (#117)

## 1.3.0
> Released Mar 03, 2019

* New configuration `dashboard-center-content` to center content (#88)
* Handle the case where dashboard lines longer than win width (#101)
* Fix shortcut regression bug (#102)
* Fix opening org mode "links" (#106)
* Add shortcut indicators to sections (#103)
* Indicate when a section has no items (#107)
* Give section headings a face for easy visual scanning (#109)

## 1.2.5
> Released Feb 25, 2019

* Improve CircleCI badge

## 1.2.4
> Released Sep 23, 2017

* Registers widget - Thanks to [Pedro Silva](https://github.com/pedros)

## 1.2.3
> Released Aug 11, 2017

* Refactored widgets into their own module
* Added org-mode agenda

## 1.0.3
> Released Dec 05, 2016

* `g` shortcut refreshes contents of Dashboard.
* Stop dashboard from displaying if Emacs is launched with a filename
* Fix whitespace-mode banner issue
