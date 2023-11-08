## Running Livebook test suite

### Unit tests

```
$ mix test
```

### Teams tests

Integration with Livebook Teams requires cloning the private Livebook Teams repository. By default it is expected to be in sibling directory to Livebook itself. But you can set `TEAMS_PATH` to a custom directory.

Livebook should take care of automatically setting up the Teams repository for you. However, if you run into issues, you can run the following in the Teams repository:

```
$ MIX_ENV=livebook mix setup
```
