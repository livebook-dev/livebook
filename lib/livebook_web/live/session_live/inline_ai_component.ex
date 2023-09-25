# TODO this may not need to be a live component at all
defmodule LivebookWeb.SessionLive.InlineAIComponent do
  require IEx
  use LivebookWeb, :live_component

  require Logger

  @impl true
  @spec update(maybe_improper_list | map, any) :: {:ok, map}
  def update(assigns, socket) do
    {:ok,
     socket
     |> assign(assigns)
     |> assign_new(:loading, fn -> false end)
     |> assign_new(:response, fn -> "" end)}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <%!-- <div class="inlineDiffViewZone" monaco-view-zone="l1" style="position: absolute; width: 100%; display: block; top: 10; height: 68px;" monaco-visible-view-zone="true">
    <div style="height: 100%; width: 100%;">
      <div tabindex="0" style="padding-top: 4px; box-sizing: border-box; outline: none;">
         <div style="z-index: 1000001; position: relative; padding: 4px 4px 0px; max-width: 500px; font-size: 12px; background-color: var(--vscode-editor-background); color: var(--vscode-foreground); border-radius: 5px; user-select: text; box-sizing: border-box; overflow: hidden auto; box-shadow: 0 4px 8px var(--vscode-interactiveEditor-shadow); margin-left: 74px; border: 1px solid var(--vscode-commandCenter-inactiveBorder);">
            <div style="position: absolute; right: -2px; top: -4px; color: var(--vscode-input-placeholderForeground); cursor: pointer; z-index: 1000002; padding: 4px;">
               <div class="codicon codicon-x" style="font-size: 10px;"></div>
            </div>
            <div style="height: 4px;"></div>
            <div style="display: flex; flex-direction: column;">
               <div style="flex-grow: 1;">
                  <div style="width: 100%; overflow: hidden;">
                     <div style="display: grid; position: relative; grid-template-columns: 1fr 1fr; width: 200%;">
                        <div autocapitalize="off" class="aislash-editor-input" contenteditable="true" spellcheck="false" data-lexical-editor="true" role="textbox" style="resize: none; grid-area: 1 / 1 / 1 / 1; overflow: hidden; line-height: inherit; font-family: inherit; font-size: inherit; color: var(--vscode-input-foreground); background-color: transparent; display: block; outline: none; box-sizing: border-box; border: none; overflow-wrap: break-word; word-break: break-word; padding: 0px 0.5rem; user-select: text; white-space: pre-wrap;">
                           <p><br></p>
                        </div>
                        <div style="grid-area: 1 / 2 / 1 / 2;">
                           <div class="aislash-editor-placeholder" style="position: relative; top: 0px; left: -100%; padding: 0px 0.5rem; pointer-events: none; user-select: none; color: var(--vscode-input-placeholderForeground);">Editing instructions... (⇅ for history, @ for code / documentation)</div>
                        </div>
                     </div>
                  </div>
               </div>
               <div style="flex-shrink: 0;">
                  <div class="inline-prompt-button-area" style="display: flex; justify-content: flex-start; align-items: center; margin: 4px 0px 6px;">
                     <div class="cursor-button cursor-button-secondary cursor-button-secondary-clickable" style="user-select: none;">Esc to close</div>
                     <div class="cursor-button cursor-button-secondary cursor-button-not-clickable" style="margin-left: auto;">⌘K to toggle focus</div>
                  </div>
               </div>
            </div>
         </div>
      </div>
    </div>
    </div> --%>

    <div id={"inline_ai-#{@id}"}  phx-hook="InlineAIComponent"
      data-cell-id={@id}>
      <%!-- <div id="uniqueid" style="height:200px"></div> --%>
      <%!-- <form phx-submit="send_message" phx-target={@myself}> --%>
      <form>
        <input autofocus type="text" name="message" class="inline_ai_input border-2" />
        <button class="button-base button-blue" type="submit">
          <.spinner class="hidden phx-submit-loading:block mr-2" />
          <span>Send</span>
        </button>
      </form>
    </div>
    """
  end

  @impl true
  def handle_event(
        "request_inline_ai_completion",
        %{"selectedCode" => selected_code, "cellContent" => cell_content, "prompt" => prompt},
        socket
      ) do
    openai_request(socket.assigns, prompt, selected_code, cell_content)
    # anthropic_request(socket.assigns, prompt, selected_code, cell_content)
    {:noreply, socket |> assign(loading: true)}
  end

  def anthropic_request(assigns, prompt, selected_code, cell_content) do
    anthropic = AnthropicEx.new(Application.fetch_env!(:livebook, :anthropic_api_key))

    messages = [
      AnthropicEx.ChatMessage.human(generate_system_prompt(prompt, selected_code, cell_content)),
      AnthropicEx.ChatMessage.human(prompt)
    ]

    parent = self()

    Task.async(fn ->
      anthropic
      |> AnthropicEx.ChatCompletion.create(%{stream: true, model: "claude-2", messages: messages})
      |> Stream.flat_map(& &1)
      |> Enum.reduce({"", false, ""}, fn token, {text, in_code_block, buffer} ->
        text = text <> token
        Logger.debug("Token: '#{token}'")
        Logger.debug(text)

        # - don't return any tokens until ```elixir\n is encountered
        # - if a token ending in ` is returned, buffer it to
        #   see if it forms part of a code ending block before returning it
        # TODO implement this as a well tested method that returns just the code tokens
        cond do
          String.ends_with?(text, "```elixir\n") || String.ends_with?(text, "```elixir") ->
            {text, true, ""}

          Regex.match?(~r/```\s*$/, text) ->
            {text, false, ""}

          String.ends_with?(text, "`") ->
            {text, in_code_block, token}

          !in_code_block ->
            {text, false, ""}

          in_code_block ->
            send(parent, {:inline_ai_response_chunk, assigns.id, text, buffer <> token})
            {text, true, ""}
        end
      end)
    end)
  end

  def create_openai_chat_req(args = [_ | _]) do
    args
    |> Enum.into(%{
      model: "gpt-4",
      temperature: 0
    })
    |> OpenaiEx.ChatCompletion.new()
  end

  def get_openai_completion_stream(openai = %OpenaiEx{}, cc_req = %{}) do
    openai
    |> OpenaiEx.ChatCompletion.create(cc_req, stream: true)
    |> Stream.flat_map(& &1)
    |> Stream.map(fn %{data: d} -> d |> Map.get("choices") |> Enum.at(0) |> Map.get("delta") end)
    |> Stream.filter(fn map -> map |> Map.has_key?("content") end)
    |> Stream.map(fn map -> map |> Map.get("content") end)
  end

  def openai_request(assigns, prompt, selected_code, cell_content) do
    openai = OpenaiEx.new(Application.fetch_env!(:livebook, :openai_api_key))

    messages = [
      OpenaiEx.ChatMessage.system(generate_system_prompt(prompt, selected_code, cell_content)),
      OpenaiEx.ChatMessage.user(prompt)
    ]

    # Logger.info "System prompt: #{system_prompt}"
    # Logger.info "User prompt: #{user_prompt}"
    #   hd(messages)
    #   tl(messages)

    # )
    Logger.info(inspect(messages))

    parent = self()

    Task.async(fn ->
      openai
      |> get_openai_completion_stream(create_openai_chat_req(messages: messages))
      |> Enum.reduce({"", false, ""}, fn token, {text, in_code_block, buffer} ->
        text = text <> token
        Logger.debug("Token: '#{token}'")
        Logger.debug(text)

        # - don't return any tokens until ```elixir\n is encountered
        # - if a token ending in ` is returned, buffer it to
        #   see if it forms part of a code ending block before returning it
        # TODO implement this as a well tested method that returns just the code tokens
        cond do
          String.ends_with?(text, "```elixir\n") ->
            {text, true, ""}

          Regex.match?(~r/```\s*$/, text) ->
            {text, false, ""}

          String.ends_with?(text, "`") ->
            {text, in_code_block, token}

          !in_code_block ->
            {text, false, ""}

          in_code_block ->
            send(parent, {:inline_ai_response_chunk, assigns.id, text, buffer <> token})
            {text, true, ""}
        end
      end)
    end)
  end

  def generate_system_prompt(_prompt, "", cell_content) do
    """
    You are an AI programming assistant that is excellent at writing idiomatic elixir code.
    Follow the user's requirements carefully & to the letter.
    You're working on a livebook computational notebook written in elixir.
    Don't introduce any new components or files.
    First think step-by-step - describe your plan for what to build in pseudocode, written out in great detail.

    Then respond with a single elixir code block with the new code to be inserted.

    Minimize any other prose.
    Keep your answers short and impersonal.
    Never create a new component or file.


    You must wrap every bit of code in an elixir code block with ```elixir, even if your resopnse is short and only contains elixir code

    At the very end of your response, after the code block, write "Peace out". This is to test that my code block detection works.

    """
  end

  # TODO for longer selections we could also ask chatgpt to return a diff
  #      like so: https://github.com/rapidpages/rapidpages/blob/main/src/server/openai.ts
  # TODO we can give much better context to the model
  def generate_system_prompt(_prompt, selected_code, cell_content) do
    """
    You are an AI programming assistant that is excellent at writing idiomatic elixir code.
    Follow the user's requirements carefully & to the letter.
    You're working on a livebook computational notebook written in elixir.
    Don't introduce any new components or files.
    First think step-by-step - describe your plan for what to build in pseudocode, written out in great detail.

    Then respond with a single elixir code block with the new code.
    Modify as few characters as possible and use as few characters as possible on the diff.
    Minimize any other prose.
    Keep your answers short and impersonal.
    Never create a new component or file.

    The code changes must always be valid elixir code.

    There can be multiple code changes.

    You must wrap every bit of code in an elixir code block with ```elixir, even if your resopnse is short and only contains elixir code

    You are modifiying a subset of the highlighted portion of following code. I am only giving you the full code for context

    ```elixir
    #{cell_content}
    ```

    The code you are meant to modify is this:

    ```elixir
    #{selected_code}
    ```

    Please do not attempt to rewrite any of the other code i shared previously. It is only for context.

    At the very end of your response, after the code block, write "Peace out". This is to test that my code block detection works.
    """
  end
end
