defmodule LiveBookWeb.Icons do
  import Phoenix.HTML.Tag
  import Phoenix.LiveView.Helpers

  @doc """
  Returns icon svg tag.
  """
  def svg(name, attrs \\ [])

  def svg(:chevron_right, attrs) do
    assigns = %{attrs: heroicon_svg_attrs(attrs)}

    ~L"""
    <%= tag(:svg, @attrs) %>
      <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M9 5l7 7-7 7" />
    </svg>
    """
  end

  def svg(:play, attrs) do
    assigns = %{attrs: heroicon_svg_attrs(attrs)}

    ~L"""
    <%= tag(:svg, @attrs) %>
      <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M14.752 11.168l-3.197-2.132A1 1 0 0010 9.87v4.263a1 1 0 001.555.832l3.197-2.132a1 1 0 000-1.664z" />
      <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M21 12a9 9 0 11-18 0 9 9 0 0118 0z" />
    </svg>
    """
  end

  def svg(:plus, attrs) do
    assigns = %{attrs: heroicon_svg_attrs(attrs)}

    ~L"""
    <%= tag(:svg, @attrs) %>
      <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M12 6v6m0 0v6m0-6h6m-6 0H6" />
    </svg>
    """
  end

  def svg(:trash, attrs) do
    assigns = %{attrs: heroicon_svg_attrs(attrs)}

    ~L"""
    <%= tag(:svg, @attrs) %>
      <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M19 7l-.867 12.142A2 2 0 0116.138 21H7.862a2 2 0 01-1.995-1.858L5 7m5 4v6m4-6v6m1-10V4a1 1 0 00-1-1h-4a1 1 0 00-1 1v3M4 7h16" />
    </svg>
    """
  end

  def svg(:chip, attrs) do
    assigns = %{attrs: heroicon_svg_attrs(attrs)}

    ~L"""
    <%= tag(:svg, @attrs) %>
      <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M9 3v2m6-2v2M9 19v2m6-2v2M5 9H3m2 6H3m18-6h-2m2 6h-2M7 19h10a2 2 0 002-2V7a2 2 0 00-2-2H7a2 2 0 00-2 2v10a2 2 0 002 2zM9 9h6v6H9V9z" />
    </svg>
    """
  end

  def svg(:information_circle, attrs) do
    assigns = %{attrs: heroicon_svg_attrs(attrs)}

    ~L"""
    <%= tag(:svg, @attrs) %>
      <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M13 16h-1v-4h-1m1-4h.01M21 12a9 9 0 11-18 0 9 9 0 0118 0z" />
    </svg>
    """
  end

  def svg(:exclamation_circle, attrs) do
    assigns = %{attrs: heroicon_svg_attrs(attrs)}

    ~L"""
    <%= tag(:svg, @attrs) %>
      <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M12 8v4m0 4h.01M21 12a9 9 0 11-18 0 9 9 0 0118 0z" />
    </svg>
    """
  end

  # https://heroicons.com
  defp heroicon_svg_attrs(attrs) do
    heroicon_svg_attrs = [
      xmlns: "http://www.w3.org/2000/svg",
      fill: "none",
      viewBox: "0 0 24 24",
      stroke: "currentColor"
    ]

    Keyword.merge(attrs, heroicon_svg_attrs)
  end
end
