Setup requires [Poetry](https://python-poetry.org/) and is then completed via

```shell
poetry install
```

Show demo via

```shell
./demo-changes.sh | poetry run python visualizer/visualizer.py
```

Run with your own turing machine implementation by piping it's output into the turing host, e.g.

```shell
/path/to/my/turing/machine | poetry run python visualizer/visualizer.py
```