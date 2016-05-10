Application dderl
=================

Release history with new or improved features and bugfixes

Version 1.5.1 (Release Date 10.05.2016)
======================================
* [Bugfixes](https://github.com/K2InformaticsGmbH/dderl/issues/237)
* Enabled possibility to gzip compression with cowboy
* dderloci 0.2.0
* JS library version added
* Timeout for sync events on dder_fsm updated to one minute

Version 1.5.0 (Release Date 26.04.2016)
======================================
* imem 1.4.15
* Using brunch as task runner
* Added bower for dependency management
* Included font awesome icons
* Fixed edit extra escape of new lines
* Added window management (minimize) to tables
* Implemented access logs
* Fixed session timeout on login page
* Implemented column menus "Shrink" and "Fit to data"
* Added hash information to statistics report

Version 1.4.1 (Release Date 06.04.2016)
======================================
* imem 1.4.14
* Added rename view and delete view actions to sql 'Edit' menu.
* Sort by count on histogram
* Added support for multiple browse data (max 10)
* Login failures cosmetic changes

Version 1.4.0 (Release Date 21.03.2016)
======================================
* Replaced alert with jquery equivalent functions
* Replaced prompt with custom javascript control
* Added app name in page title
* Added error message trying to delete rename invalid views

Version 1.3.19 (Release Date 14.03.2016)
======================================
* Added background svg image
* imem 1.4.12

Version 1.3.18 (Release Date 05.03.2016)
======================================
* Bug fix on [issue 156](https://github.com/K2InformaticsGmbH/dderl/issues/156)
* imem 1.4.11

Version 1.3.17 (Release Date 01.03.2016)
======================================
* Export CSV bug fix [#173](https://github.com/K2InformaticsGmbH/dderl/issues/173)
* imem 1.4.10

Version 1.3.16 (Release Date 26.02.2016)
======================================
* imem 1.4.9

Version 1.3.15 (Release Date 18.02.2016)
======================================
* imem 1.4.8

Version 1.3.14 (Release Date 12.02.2016)
======================================
* Histogram with multiple columns result separated
* Added clear button to avoid confusion with delete connections
* Fixed privileges for delete connections
* Added confirmation to delete connections
* Configurable export csv delimiter
* imem 1.4.7

Version 1.3.13 (Release Date 05.02.2016)
======================================
* Added support for histogram using multiple columns
* imem 1.4.6

Version 1.3.12 (Release Date 29.01.2016)
======================================
* imem 1.4.5

Version 1.3.11 (Release Date 26.01.2016)
======================================
* imem 1.4.4

Version 1.3.10 (Release Date 19.01.2016)
======================================
* imem 1.4.3
* erlimem 1.2.5
* Added complexity check for passwords

Version 1.3.9 (Release Date 14.12.2015)
======================================
* imem 1.4.2
* export to CSV from bind Oracle-SQL

Version 1.3.8 (Release Date 10.12.2015)
======================================
* imem 1.4.1

Version 1.3.7 (Release Date 07.12.2015)
======================================
* imem 1.4.0
* Cosmetics and minor bug fixes

Version 1.0.9 (Release Date 05.02.2015)
======================================
* Added support for multiple sql queries
* Improved cell selection using hotkeys
* Added parameterized query support
* [Bug 37] Fixed wrong focus after refresh on result window
* Added column alias to histogram result
* Fixed support for # in pretty printed sql editor
* imem from 1.2.5 to 1.2.6
* dderloci from 0.0.3 to 0.0.4
* erlimem from 1.0.5 to 1.0.8
* removed dependencies included by imem
* improved go to end increasing the size of last block to 300

Version 1.0.7 (Release Date 17.11.2014)
=======================================
* [Bug 10] Views grouped by target DB SBS0, SBS1, SBS2
* [Bug 11] Fixed errors with formatters preventing query execution
* [Bug 24] Implemented access to the system with users without administrative rights
* Generalized erlang term editor, now it support any text in read only format
* Implemented copy in JSON format and including the header
* Added dashboard functionality
* Improved ctrl+click selection to not clear the selection in case of small mouse drags
* Minimum size of columns set to fixed 40 pixels instead of proportional to the column name
* Improved security by better randomization of session tokens.
* Restricted listener to not allow sslv3 due to poodle vulnerability
* Improved paste performance and made in steps to not block the ui on big amount of data
* erlang version from R16B03(erts 5.10.4) to 17(erts 6.2)
* goldrush from 0.1.0 to 0.1.6
* lager from 2.0.0 to 2.0.3
* ranch from 0.8.4 to 1.0.0
* cowboy from 0.8.4 to 1.0.0
* imem from 1.2.3 to 1.2.5
* erlimem from 1.0.4 to 1.0.5
* sqlparse from 1.0.0 to 1.2.2
* new library jpparse 0.0.3 added

Version 1.0.6 (Release Date 04.09.2013)
=======================================
* Fixed support to table operations on multi selection.
* Delete key now clear the content of the cells unless the full row is selected

Version 1.0.5 (Release Date 28.08.2013)
=======================================

* numeric columns are right aligned
* disconnecting from the system clean up all the session related processes in a shorter time
* added a Window entry on the menu to allow easier selection of open tables
* added support to perform table operations using multiple selection

* fix click on a disabled table behind the erlang term editor steal the focus

Version 1.0.4 (Release Date 08.08.2013)
=======================================

* added table operations drop, truncate, snapshot and restore as context menu of the data
* erlang term editor now support functions inside binaries and preserve comments
* implemented auto edit replacing the content of the current cell on key press
* Enter and f2 keys set the cell to edit mode, positioning the caret at the end of the text-box
* Enter key when the cell is in edit mode, accept the new value and move to the next cell
* added support to have more than one table in edit mode at the time
* shift + arrows keys selection improved to select range instead of full rows
* implemented keep-alive ping mechanism to avoid losing the session if the browser is active
* initial view shown can be redefined by the user

* fix focus not being recover after scrolling
* fix drag bug when tables are bigger than the available space
* fix erlang term editor crash on empty tuples
* fix error after reload a table with an active cell editor

Version 1.0.3 (Release Date 09.07.2013)
=======================================

* check for compatible versions of the dependencies running
* improved positioning of sort windows and sql editor
* control characters in the data are now shown as utf-8 escaped characters

* fix sorting by columns not presented in the table
* fix crash due to invalid utf-8 encoded data
* fix range selection

Version 1.0.2 (Release Date 25.06.2013)
=======================================

* added about dialog to get information about the applications in the system
* implemented erlang term editor with automatic formatting
* included support for Internet Explorer 9

* fix right click error on empty row
