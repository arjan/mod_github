Zotonic Github webhook
----------------------

This is a Zotonic module to trigger a rebuild of your site from the
API.

The module exposes an API service at `/api/github/webhook` which can
be used to POST a token to which, if the token is correct, triggers a
"git pull", rebuilds Zotonic and flushes the site's depcache.

The token is a shared secret stored in the `mod_github.webhook_token`
configuration value, and can be passed as a POST parameter or in the
query string.


Arjan Scherpenisse, 2013

### Known issues
#### SSH passphrase not supported
Zotonic will not store your passphrase. The solution is to create a separate SSH key for the zotonic user on the server - without pass phrase - and use this key as deploy key on GitHub.
