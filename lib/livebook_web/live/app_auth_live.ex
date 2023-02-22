defmodule LivebookWeb.AppAuthLive do
  use LivebookWeb, :live_view

  @impl true
  def mount(%{"slug" => slug}, _session, socket) when not socket.assigns.app_authenticated? do
    {:ok, assign(socket, slug: slug, password: "", errors: [])}
  end

  def mount(%{"slug" => slug}, _session, socket) do
    {:ok, push_navigate(socket, to: ~p"/apps/#{slug}")}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="h-screen flex items-center justify-center" id="app-auth" phx-hook="AppAuth">
      <div class="flex flex-col space-y-4 items-center">
        <a href={~p"/"}>
          <img src={~p"/images/logo.png"} height="128" width="128" alt="livebook" />
        </a>
        <div class="text-2xl text-gray-800">
          This app is password-protected
        </div>

        <div class="max-w-2xl text-center text-gray-700">
          <span>Type the app password to access it or</span>
          <a
            class="border-b border-gray-700 hover:border-none"
            href={~p"/authenticate?redirect_to=#{~p"/apps/#{@slug}"}"}
          >login into Livebook</a>.
        </div>
        <div class="text-2xl text-gray-800 w-full pt-2">
          <form class="flex flex-col space-y-4 items-center" phx-submit="authenticate">
            <div phx-feedback-for="password" class={["w-[20ch]", @errors != [] && "show-errors"]}>
              <input
                type="password"
                name="password"
                class="input"
                value={@password}
                placeholder="Password"
                autofocus
              />
              <span
                :for={error <- @errors}
                class="mt-1 hidden text-red-600 text-sm phx-form-error:block"
              >
                <%= translate_error(error) %>
              </span>
            </div>
            <button type="submit" class="button-base button-blue">
              Authenticate
            </button>
          </form>
        </div>
      </div>
    </div>
    """
  end

  @impl true
  def handle_event("authenticate", %{"password" => password}, socket) do
    socket =
      if LivebookWeb.AppAuthHook.valid_password?(password, socket.assigns.app_settings) do
        token = LivebookWeb.AppAuthHook.get_auth_token(socket.assigns.app_settings)
        push_event(socket, "persist_app_auth", %{"slug" => socket.assigns.slug, "token" => token})
      else
        assign(socket, password: password, errors: [{"app password is invalid", []}])
      end

    {:noreply, socket}
  end

  def handle_event("app_auth_persisted", %{}, socket) do
    {:noreply, push_navigate(socket, to: ~p"/apps/#{socket.assigns.slug}")}
  end
end
