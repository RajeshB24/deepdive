## Test environments
* local OS X install, R 3.5.3
* ubuntu 14.04 (on travis-ci), R 3.5.3
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings  | 1 notes 

* This is a new release. 

Resubmission Notes:

In addition to below fixes few more bug fixes has been done.

Fixed for below 4 comments :(Fixes: 1.added https link in description field
                                    2. print replaced with message /warnings
                                    3. removed donotrun clause
                                    4. made examples less than 5 secs
                                    


1.If there are references describing the methods in your package, please
add these in the description field of your DESCRIPTION file in the form
authors (year) <doi:...>
authors (year) <arXiv:...>
authors (year, ISBN:...)
or if those are not available: <https:...>
with no space after 'doi:', 'arXiv:', 'https:' and angle brackets for
auto-linking.
(If you want to add a title as well please put it in quotes: "Title")

2.You write information messages to the console that cannot be easily
suppressed.
It is more R like to generate objects that can be used to extract the
information a user is interested in, and then print() that object.
Instead of print()/cat() rather use message()/warning()  or
if(verbose)cat(..) if you really have to write text to the console.
(except for print() and summary() functions)

3."donotrun clause"" should only be used if the example really cannot be executed
(e.g. because of missing additional software, missing API keys, ...) by
the user. That's why wrapping examples in \dontrun{} adds the comment
("# Not run:") as a warning for the user.
Does not seem necessary.

4.Please unwrap the examples if they are executable in < 5 sec, or create
additionally small toy examples to allow automatic testing.
(You could also replace \dontrun{} with \donttest if it takes longer
than 5 sec to be executed, but it would be preferable to have automatic
checks for functions.)
