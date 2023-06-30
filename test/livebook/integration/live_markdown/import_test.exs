defmodule Livebook.Integration.LiveMarkdown.ImportTest do
  use Livebook.TeamsIntegrationCase, async: true

  alias Livebook.LiveMarkdown.Import
  alias Livebook.Notebook

  describe "notebook stamp" do
    test "restores hub secret names from notebook stamp using offline hub" do
      hub =
        build(:team,
          id: "team-org-number-2946",
          teams_key: "AleIxOFlwSiOS78WXtVU01ySmitjzy-5pAuCh4i1wZE",
          org_public_key:
            "MIIBCgKCAQEA2uRttEa6UvtiAUhv-MhPZvvlrCNeeL5n6oP4pliqoMBD7vsi4EvwnrqjCCicwHeT4y8Pu1kmzTelDAHEyO8alllBtfnZnQkPOqo1Y6c6qBHhcioc2FrNvdAydMiByhyn_aqNbFNeMMgy9ogHerAQ6XPrGSaXEvIcWn3myz-zxYdeEDW5G5W95o7Q0x7lokdVBUwXbazH0JVu_-1FUr7aOSjjuNHX6rXMRA3wr4n2SuhGOvihrX5IYRb733pae2aTOfJZGD_83eUPHTu_cPoUflcvIPtnVlGTxBgSX9Ayl1X3uDOnJsk2pxawFF6GxBMUKjMGyGDTg_lL45cgsWovXQIDAQAB",
          hub_name: "org-number-2946"
        )

      Livebook.Hubs.set_offline_hub(hub)

      # Generated with:

      # %{
      #   Notebook.new()
      #   | name: "My Notebook",
      #     hub_id: hub.id,
      #     sections: [
      #       %{
      #         Notebook.Section.new()
      #         | name: "Section 1",
      #           cells: [
      #             %{
      #               Notebook.Cell.new(:code)
      #               | source: """
      #                 IO.puts("hey")
      #                 """
      #             }
      #           ]
      #       }
      #     ],
      #     hub_secret_names: ["DB_PASSWORD"]
      # }
      # |> Livebook.LiveMarkdown.Export.notebook_to_livemd()
      # |> elem(0)
      # |> IO.puts()

      markdown = """
      <!-- livebook:{"hub_id":"team-org-number-2946"} -->

      # My Notebook

      ## Section 1

      ```elixir
      IO.puts("hey")
      ```

      <!-- livebook:{"offset":111,"stamp":{"token":"QTEyOEdDTQ.yw3drh2WcwU8K6jS9Wp0HPupyX3qoc8iBmUXrMVKvSPnIOGEYMmu160e89E.xyzsr7PxSBrA8Elt.N3KyvcuTrFyMYpSl8WB1Sctv-1YjSjv_DCZoOVje_zXPYpm4iV_Ss5tVUSA7IWE.lV7grc6HYOYJrf0YYScPwQ","token_signature":"KSd-EhXw2CrmS9m4aZnPhTgWzlNdQNJ0wvYmuNvi8Pxaqb-prKO0FN_BTcPHtk4ZDHJaIFac-8dyefkCHpIElAc_N7vExgO9_7wSOJ8Hagip7DOxOBfqcR6iC17ejiw-2wWFJu0p6deaXpm2RWkWJU--wiU1cAHoKoJGqIsMMxNmgAkT44Pok0ni5BtnTfZjq_c2iPTYfP-8uU2WFIDmzEeOL-He5iWNUlixnf5Aj1YSVNldi6vTtR70xBRvlUxPCkWbt1x6XjanspY15j43PgVTo0EPM4kGCkS2HcWBZB_XscxZ4-V-WdpQ0pkv1goPdfDGDcAbjP7z8oum9_ZKNA","version":1}} -->
      """

      {notebook, []} = Import.notebook_from_livemd(markdown)

      assert %Notebook{hub_id: "personal-hub", hub_secret_names: ["DB_PASSWORD"]} = notebook
    end

    test "returns a warning when notebook stamp is invalid using offline hub" do
      hub =
        build(:team,
          id: "team-org-number-2946",
          teams_key: "AleIxOFlwSiOS78WXtVU01ySmitjzy-5pAuCh4i1wZE",
          org_public_key:
            "MIIBCgKCAQEA2uRttEa6UvtiAUhv-MhPZvvlrCNeeL5n6oP4pliqoMBD7vsi4EvwnrqjCCicwHeT4y8Pu1kmzTelDAHEyO8alllBtfnZnQkPOqo1Y6c6qBHhcioc2FrNvdAydMiByhyn_aqNbFNeMMgy9ogHerAQ6XPrGSaXEvIcWn3myz-zxYdeEDW5G5W95o7Q0x7lokdVBUwXbazH0JVu_-1FUr7aOSjjuNHX6rXMRA3wr4n2SuhGOvihrX5IYRb733pae2aTOfJZGD_83eUPHTu_cPoUflcvIPtnVlGTxBgSX9Ayl1X3uDOnJsk2pxawFF6GxBMUKjMGyGDTg_lL45cgsWovXQIDAQAB",
          hub_name: "org-number-2946"
        )

      Livebook.Hubs.set_offline_hub(hub)

      markdown = """
      <!-- livebook:{"hub_id":"team-org-number-2946"} -->

      # My Notebook

      ## Section 1

      ```elixir
      IO.puts("hey")
      ```

      <!-- livebook:{"offset":58,"stamp":{"token":"invalid","token_signature":"invalid","version":1}} -->
      """

      assert {%Notebook{hub_secret_names: []}, ["failed to verify notebook stamp"]} =
               Import.notebook_from_livemd(markdown)
    end
  end
end
