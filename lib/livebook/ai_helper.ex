# TODO this module needs work to support back-and-forth conversations
defmodule Livebook.AiHelper do
  # TODO lots of arguments ... maybe more ergonomical to use keyword args or a map here?
  # TODO work out how to refactor this so I can easily get access to the whole
  # livebook context (including other cells, their output, etc)
  def stream_completion(
        "openai",
        prompt,
        code_before_selection,
        selected_code,
        code_after_selection
      ) do
    Livebook.AiHelper.Openai.stream_completion(
      prompt,
      code_before_selection,
      selected_code,
      code_after_selection
    )
  end

  def stream_completion(
        "anthropic",
        prompt,
        code_before_selection,
        selected_code,
        code_after_selection
      ) do
    Livebook.AiHelper.Anthropic.stream_completion(
      prompt,
      code_before_selection,
      selected_code,
      code_after_selection
    )
  end

  require Logger
  # TODO there is probably a much simpler and more robust way to do this...
  def filter_code_tokens(unfiltered_stream) do
    unfiltered_stream
    |> Stream.scan({"", "", false, ""}, fn token,
                                           {_prev_token, text, in_code_block, buffer} = acc ->
      Logger.debug("Token: '#{token}'")
      Logger.debug(inspect(acc))

      text = text <> token

      Logger.info(text)

      #   # - don't return any tokens until ```elixir\n is encountered
      #   # - if a token ending in ` is returned, buffer it to
      #   #   see if it forms part of a code ending block before returning it
      cond do
        String.ends_with?(text, "```elixir\n") || String.ends_with?(text, "```elixir") ->
          {"", text, true, ""}

        Regex.match?(~r/```\s*$/, text) ->
          {"", text, false, ""}

        String.ends_with?(text, "`") ->
          {"", text, in_code_block, token}

        !in_code_block ->
          {"", text, false, ""}

        in_code_block ->
          {buffer <> token, text, true, ""}
      end
    end)
    |> Stream.map(&elem(&1, 0))
    |> Stream.reject(&(&1 == ""))
  end

  def shared_prompt do
    """
    You are the Livebook AI helper, a world-class programmer that can complete any goal by writing idiomatic elixir code.
    Follow the user's requirements carefully & to the letter.
    You're working in a livebook computational notebook.
    Never introduce any new components or files.
    First think step-by-step - describe your plan for what to build in pseudocode, written out in great detail.
    Then respond with a single elixir code block with the new code to be inserted.
    Minimize any other prose.
    Keep your answers short and impersonal.
    You must wrap every bit of code in an elixir code block with ```elixir, even if your resopnse is short and only contains elixir code
    At the very end of your response, after the code block, write "Peace out". This is to test that my code block detection works.
    """
  end

  def new_code_prompt(code_before_selection, code_after_selection) do
    """
    Your task is to write new code that fits into the given code context.

    This is the code context just before where your code will be inserted. You can't modify this code. Do not print out bits of this context in your response.

    ```elixir
    #{code_before_selection}
    ```

    This is the code context after the bit you are inserting. Do not modify this. Do not print it in your response.

    ```elixir
    #{code_after_selection}
    ```
    """
  end

  def modify_existing_code_prompt(code_before_selection, selected_code, code_after_selection) do
    """
    Your task is to modify the selected piece of code.

    This is the code before the code you are meant to modify. You can't modify this code. Do not print out bits of this context in your response.

    ```elixir
    #{code_before_selection}
    ```

    This is the selected code you are meant to modify. The only code in your response should be a modification of this code.

    ```elixir
    #{selected_code}
    ```

    This is the code context after the bit you are changing. Do not modify this. Do not print it in your response.

    ```elixir
    #{code_after_selection}
    ```
    """
  end

  def generate_system_prompt(code_before_selection, "", code_after_selection) do
    """
    #{shared_prompt()}

    #{new_code_prompt(code_before_selection, code_after_selection)}
    """
  end

  def generate_system_prompt(code_before_selection, selected_code, code_after_selection) do
    """
    #{shared_prompt()}

    #{modify_existing_code_prompt(code_before_selection, selected_code, code_after_selection)}
    """
  end
end
