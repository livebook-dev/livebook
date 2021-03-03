defmodule LiveBookWeb.Icons do
  import Phoenix.HTML.Tag
  import Phoenix.LiveView.Helpers

  @doc """
  Returns icon svg tag.
  """
  def svg(name, attrs \\ [])

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

  def svg(:question_mark_circle, attrs) do
    assigns = %{attrs: heroicon_svg_attrs(attrs)}

    ~L"""
    <%= tag(:svg, @attrs) %>
      <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M8.228 9c.549-1.165 2.03-2 3.772-2 2.21 0 4 1.343 4 3 0 1.4-1.278 2.575-3.006 2.907-.542.104-.994.54-.994 1.093m0 3h.01M21 12a9 9 0 11-18 0 9 9 0 0118 0z" />
    </svg>
    """
  end

  def svg(:pencil, attrs) do
    assigns = %{attrs: heroicon_svg_attrs(attrs)}

    ~L"""
    <%= tag(:svg, @attrs) %>
      <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M15.232 5.232l3.536 3.536m-2.036-5.036a2.5 2.5 0 113.536 3.536L6.5 21.036H3v-3.572L16.732 3.732z" />
    </svg>
    """
  end

  def svg(:folder, attrs) do
    assigns = %{attrs: heroicon_svg_attrs(attrs)}

    ~L"""
    <%= tag(:svg, @attrs) %>
      <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M3 7v10a2 2 0 002 2h14a2 2 0 002-2V9a2 2 0 00-2-2h-6l-2-2H5a2 2 0 00-2 2z" />
    </svg>
    """
  end

  def svg(:document_text, attrs) do
    assigns = %{attrs: heroicon_svg_attrs(attrs)}

    ~L"""
    <%= tag(:svg, @attrs) %>
      <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M9 12h6m-6 4h6m2 5H7a2 2 0 01-2-2V5a2 2 0 012-2h5.586a1 1 0 01.707.293l5.414 5.414a1 1 0 01.293.707V19a2 2 0 01-2 2z" />
    </svg>
    """
  end

  def svg(:check_circle, attrs) do
    assigns = %{attrs: heroicon_svg_attrs(attrs)}

    ~L"""
    <%= tag(:svg, @attrs) %>
      <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M9 12l2 2 4-4m6 2a9 9 0 11-18 0 9 9 0 0118 0z" />
    </svg>
    """
  end

  def svg(:dots_circle_horizontal, attrs) do
    assigns = %{attrs: heroicon_svg_attrs(attrs)}

    ~L"""
    <%= tag(:svg, @attrs) %>
      <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M8 12h.01M12 12h.01M16 12h.01M21 12a9 9 0 11-18 0 9 9 0 0118 0z" />
    </svg>
    """
  end

  def svg(:home, attrs) do
    assigns = %{attrs: heroicon_svg_attrs(attrs)}

    ~L"""
    <%= tag(:svg, @attrs) %>
      <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M3 12l2-2m0 0l7-7 7 7M5 10v10a1 1 0 001 1h3m10-11l2 2m-2-2v10a1 1 0 01-1 1h-3m-6 0a1 1 0 001-1v-4a1 1 0 011-1h2a1 1 0 011 1v4a1 1 0 001 1m-6 0h6" />
    </svg>
    """
  end

  def svg(:stop, attrs) do
    assigns = %{attrs: heroicon_svg_attrs(attrs)}

    ~L"""
    <%= tag(:svg, @attrs) %>
      <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M21 12a9 9 0 11-18 0 9 9 0 0118 0z" />
      <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M9 10a1 1 0 011-1h4a1 1 0 011 1v4a1 1 0 01-1 1h-4a1 1 0 01-1-1v-4z" />
    </svg>
    """
  end

  def svg(:chevron_up, attrs) do
    assigns = %{attrs: heroicon_svg_attrs(attrs)}

    ~L"""
    <%= tag(:svg, @attrs) %>
      <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M5 15l7-7 7 7" />
    </svg>
    """
  end

  def svg(:chevron_down, attrs) do
    assigns = %{attrs: heroicon_svg_attrs(attrs)}

    ~L"""
    <%= tag(:svg, @attrs) %>
      <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M19 9l-7 7-7-7" />
    </svg>
    """
  end

  def svg(:adjustments, attrs) do
    assigns = %{attrs: heroicon_svg_attrs(attrs)}

    ~L"""
    <%= tag(:svg, @attrs) %>
      <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M12 6V4m0 2a2 2 0 100 4m0-4a2 2 0 110 4m-6 8a2 2 0 100-4m0 4a2 2 0 110-4m0 4v2m0-6V4m6 6v10m6-2a2 2 0 100-4m0 4a2 2 0 110-4m0 4v2m0-6V4" />
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
