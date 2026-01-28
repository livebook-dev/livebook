# Script to find Livebook notebooks with duplicate slugs
#
# This script uses Livebook's own LiveMarkdown module to properly parse
# notebooks and extract the title and slug from app settings.
#
# Usage:
#   mix run find_duplicate_slugs.exs <directory>
#
# Example:
#   mix run find_duplicate_slugs.exs ./lib/livebook/notebook/learn
#   mix run find_duplicate_slugs.exs ~/my-notebooks

defmodule DuplicateSlugFinder do
  @doc """
  Finds all .livemd files in the given directory recursively.
  """
  def find_notebooks(directory) do
    directory
    |> Path.expand()
    |> Path.join("**/*#{Livebook.LiveMarkdown.extension()}")
    |> Path.wildcard()
  end

  @doc """
  Processes a notebook file and returns its metadata using Livebook's parser.
  """
  def process_notebook(path) do
    markdown = File.read!(path)
    {notebook, _info} = Livebook.LiveMarkdown.notebook_from_livemd(markdown)

    %{
      path: path,
      slug: notebook.app_settings.slug,
      title: notebook.name || "(no title)"
    }
  end

  @doc """
  Groups notebooks by app settings slug and filters to only those with duplicates.
  Only includes notebooks that have a slug defined.
  """
  def find_duplicates(notebooks) do
    notebooks
    |> Enum.filter(& &1.slug)
    |> Enum.group_by(& &1.slug)
    |> Enum.filter(fn {_slug, entries} -> length(entries) > 1 end)
    |> Enum.sort_by(fn {slug, _} -> slug end)
  end

  @doc """
  Formats and prints duplicate entries.
  """
  def print_duplicates(duplicates) do
    if Enum.empty?(duplicates) do
      IO.puts("No duplicate slugs found.")
    else
      IO.puts("Found #{length(duplicates)} slug(s) with duplicates:\n")

      for {slug, notebooks} <- duplicates do
        IO.puts("Slug: #{slug}")
        IO.puts(String.duplicate("-", 60))

        for notebook <- Enum.sort_by(notebooks, & &1.path) do
          IO.puts("  File:  #{notebook.path}")
          IO.puts("  Title: #{notebook.title}")
          IO.puts("")
        end
      end
    end
  end

  def run(directory) do
    unless File.dir?(directory) do
      IO.puts(:stderr, "Error: '#{directory}' is not a valid directory")
      System.halt(1)
    end

    notebooks = find_notebooks(directory)

    if Enum.empty?(notebooks) do
      IO.puts("No #{Livebook.LiveMarkdown.extension()} files found in '#{directory}'")
      System.halt(0)
    end

    IO.puts("Scanning #{length(notebooks)} notebook(s) in '#{directory}'...\n")

    processed =
      notebooks
      |> Enum.map(fn path ->
        try do
          process_notebook(path)
        rescue
          e ->
            IO.puts(:stderr, "Warning: Failed to parse #{path}: #{Exception.message(e)}")
            nil
        end
      end)
      |> Enum.reject(&is_nil/1)

    notebooks_with_slug = Enum.count(processed, & &1.slug)
    notebooks_without_slug = length(processed) - notebooks_with_slug

    IO.puts("Notebooks with app settings slug: #{notebooks_with_slug}")
    IO.puts("Notebooks without app settings slug: #{notebooks_without_slug}\n")

    duplicates = find_duplicates(processed)
    print_duplicates(duplicates)
  end
end

# Main entry point
case System.argv() do
  [directory | _] ->
    DuplicateSlugFinder.run(directory)

  [] ->
    IO.puts(:stderr, """
    Find Livebook notebooks with duplicate app settings slugs.

    Usage:
      mix run find_duplicate_slugs.exs <directory>

    Examples:
      mix run find_duplicate_slugs.exs ./lib/livebook/notebook/learn
      mix run find_duplicate_slugs.exs ~/my-notebooks

    This script parses notebooks using Livebook's LiveMarkdown module and
    groups them by the slug defined in their app settings. Only notebooks
    with a defined app settings slug are considered for duplicate detection.
    """)
    System.halt(1)
end
