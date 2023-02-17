defmodule LivebookWeb.AppAuthLive do
  use LivebookWeb, :live_view

  @impl true
  def mount(%{"slug" => slug}, _session, socket) when not socket.assigns.app_authenticated? do
    case Livebook.Apps.fetch_settings_by_slug(slug) do
      {:ok, app_settings} ->
        {:ok, assign(socket, slug: slug, app_settings: app_settings, password: "", errors: [])}

      :error ->
        {:ok, redirect(socket, to: Routes.home_path(socket, :page))}
    end
  end

  def mount(%{"slug" => slug}, _session, socket) do
    {:ok, push_navigate(socket, to: Routes.app_path(socket, :page, slug))}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="h-screen flex items-center justify-center" id="app-auth" phx-hook="AppAuth">
      <div class="flex flex-col space-y-4 items-center">
        <a href={Routes.path(@socket, "/")}>
          <img
            src={Routes.static_path(@socket, "/images/logo.png")}
            height="128"
            width="128"
            alt="livebook"
          />
        </a>
        <div class="text-2xl text-gray-800">
          Authentication required
        </div>

        <div class="max-w-2xl text-center text-gray-700">
          <span>Type password to access the app or</span>
          <a
            class="border-b border-gray-700 hover:border-none"
            href={
              Routes.auth_path(@socket, :index, redirect_to: Routes.app_path(@socket, :page, @slug))
            }
          >sign into Livebook</a>.
        </div>
        <div class="text-2xl text-gray-800 w-full pt-2">
          <form class="flex flex-col space-y-4 items-center" phx-submit="authenticate">
            <div
              phx-feedback-for="password"
              class={"w-[20ch] #{if(@errors != [], do: "show-errors")}"}
            >
              <input
                type="password"
                name="password"
                class="input"
                value={@password}
                placeholder="Password"
                autofocus
              />
              <%= for error <- @errors do %>
                <span class="mt-1 hidden text-red-600 text-sm phx-form-error:block">
                  <%= translate_error(error) %>
                </span>
              <% end %>
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
    {:noreply, push_navigate(socket, to: Routes.app_path(socket, :page, socket.assigns.slug))}
  end
end
