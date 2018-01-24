## Example Server

This is a small example project showing a
[Servant](http://haskell-servant.readthedocs.io/en/stable/) based web server along with the
Composite libraries and [Opaleye](http://github.com/tomjaguarpaw/haskell-opaleye/) talking to
[PostgreSQL](https://www.postgresql.org).

### Running the Server

1. Start PostgreSQL. If you don't have one running, install it via Nix, brew, your package manager,
   download, etc. Then do these two commands to initialize an empty data directory and run
   PostgreSQL:

   1. `initdb exampledb`
   2. `postgres -D exampledb`

2. Create the `example` database and schema using the provided script `example.sql`: `psql postgres
   < example.sql`

3. Compile the example server if you haven't already using `stack build`. If you don't have
   [stack](https://docs.haskellstack.org/en/stable/README/), you'll need to install that as well.

   * A few warnings about hpack versions are a-okay.

4. Run the server via `stack exec myawesomeserver-exe`. It should start listening on port 8080.

5. You can now access the API with `curl` or whatever you like to make HTTP requests with. For
   example, `curl http://localhost:8080/users`.

### Swagger

You can access the Swagger documentation at http://localhost:8080/. Assuming you have the Swagger
code generation JAR, you can generate a Python client as well, using the `generate-swagger.hs`
script in `example/scripts/`:

```bash
stack example/scripts/generate-swagger.hs \
  --output-dir example/client/ \
  --swagger-codegen-jar-path <path-to-jar>
```

Test out the client (first `cd` to `example/client/myawesomeserver_gen`):

```python
>>> from myawesomeserver import api, api_client, configuration
>>> config = configuration.Configuration()
>>> config.host = 'http://localhost:8080'
>>> client = api_client.ApiClient(configuration=config)
>>> my_api = api.DefaultApi(api_client=client)
>>> my_api.users_get()
[{'login': 'string', 'usertype': 'Owner'}, {'login': 'Cathy', 'usertype': 'Regular'}]
```
