defmodule Livebook.AiHelper.Anthropic do
  @model "claude-2"
  @temperature 0

  require Logger

  def stream_completion(
        prompt,
        code_before_selection,
        selected_code,
        code_after_selection
      ) do
    anthropic = AnthropicEx.new(Application.fetch_env!(:livebook, :anthropic_api_key))

    messages = [
      AnthropicEx.ChatMessage.human(
        generate_system_prompt(prompt, code_before_selection, selected_code, code_after_selection)
      )
    ]

    Logger.info(
      generate_system_prompt(prompt, code_after_selection, selected_code, code_after_selection)
    )

    anthropic
    |> AnthropicEx.ChatCompletion.create(%{
      stream: true,
      model: @model,
      temperature: @temperature,
      messages: messages
    })
  end

  # TODO for longer selections we could also ask chatgpt to return a diff
  #      like so: https://github.com/rapidpages/rapidpages/blob/main/src/server/openai.ts
  # TODO we need to give context of other cells (and outputs) in the notebook
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

  def generate_system_prompt(prompt, code_before_selection, "", code_after_selection) do
    """
    #{shared_prompt()}

    #{new_code_prompt(code_before_selection, code_after_selection)}

    #{prompt}
    """
  end

  def generate_system_prompt(prompt, code_before_selection, selected_code, code_after_selection) do
    """
    #{shared_prompt()}

    #{modify_existing_code_prompt(code_before_selection, selected_code, code_after_selection)}

    #{prompt}
    """
  end
end
