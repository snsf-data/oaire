# oaire 0.0.2

* New `oag_parse_object()` function allowing to parse objects returned by `oag_fetch()` and to transform them into a more readable `tibble` format.
* Objects returned by `oag_fetch()` inherit from an `"oag_object"` class that can be used by `oag_parse_object()` to know the type of object to parse.
* Fetching data with paging (offset- or cursor-based) or fetching a single page always returns the "results" part of the object returned by the OpenAIRE Graph API, with the number of records identified in the Graph being stored as an "numFound" attribute.

# oaire 0.0.1

* First minimal working version.
