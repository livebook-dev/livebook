import * as monaco from "monaco-editor/esm/vs/editor/editor.api";
import ElixirLanguageConfiguration from "./elixir/language_configuration";
import ElixirOnTypeFormattingEditProvider from "./elixir/on_type_formatting_edit_provider";

// Register the Elixir language and add relevant configuration
monaco.languages.register({ id: "elixir" });

monaco.languages.setLanguageConfiguration(
  "elixir",
  ElixirLanguageConfiguration
);

monaco.languages.registerOnTypeFormattingEditProvider(
  "elixir",
  ElixirOnTypeFormattingEditProvider
);

// TODO: add Monarch tokenizer for syntax highlighting

export default monaco;
