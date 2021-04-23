erl_docs_available? = Code.fetch_docs(:gen_server) != {:error, :chunk_not_found}

exclude = []
exclude = if erl_docs_available?, do: exclude, else: Keyword.put(exclude, :erl_docs, true)

ExUnit.start(assert_receive_timeout: 300, exclude: exclude)
