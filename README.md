SMSDirect
=========

This library provides functions to SMSDirect API.

Simple usage:
<pre>
-- get DB list
r &lt;- smsdirect "test" "test" getDB

-- form url or request (for tracing purposes)
let cmd = url "test" "test" getDB
</pre>
