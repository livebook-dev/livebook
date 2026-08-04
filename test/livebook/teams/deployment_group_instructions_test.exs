defmodule Livebook.Teams.DeploymentGroupInstructionsTest do
  use ExUnit.Case, async: true

  alias Livebook.Teams.DeploymentGroupInstructions

  @image "ghcr.io/livebook-dev/livebook:latest"

  describe "docker/2" do
    test "quotes environment variables" do
      env = [
        {"LIVEBOOK_AGENT_NAME", "default"},
        {"MALICIOUS", ~s/x" ; touch \/tmp\/pwned ; echo "/}
      ]

      assert DeploymentGroupInstructions.docker(@image, env) == """
             docker run -p 8080:8080 -p 8081:8081 --pull always \\
               -e 'LIVEBOOK_AGENT_NAME=default' \\
               -e 'MALICIOUS=x" ; touch /tmp/pwned ; echo "' \\
               #{@image}
             """
    end

    test "escapes single quotes in environment variable names and values" do
      env = [
        {"MALICIOUS", "x' ; touch /tmp/pwned ; echo '"},
        {"MALICIOUS' ; touch /tmp/pwned ; '", "value"}
      ]

      assert DeploymentGroupInstructions.docker(@image, env) == """
             docker run -p 8080:8080 -p 8081:8081 --pull always \\
               -e 'MALICIOUS=x'\\'' ; touch /tmp/pwned ; echo '\\''' \\
               -e 'MALICIOUS'\\'' ; touch /tmp/pwned ; '\\''=value' \\
               #{@image}
             """
    end
  end

  describe "fly/4" do
    test "quotes environment variables" do
      env = [
        {"LIVEBOOK_AGENT_NAME", "default"},
        {"MALICIOUS", ~s/x" ; touch \/tmp\/pwned ; echo "/}
      ]

      %{step_two: step_two} = DeploymentGroupInstructions.fly(@image, env, "my-org", "my-group")

      assert step_two == """
             fly secrets set \\
               'LIVEBOOK_AGENT_NAME=default' \\
               'MALICIOUS=x" ; touch /tmp/pwned ; echo "'

             fly deploy --ha=false
             """
    end

    test "escapes single quotes in environment variable names and values" do
      env = [
        {"MALICIOUS", "x' ; touch /tmp/pwned ; echo '"},
        {"MALICIOUS' ; touch /tmp/pwned ; '", "value"}
      ]

      %{step_two: step_two} = DeploymentGroupInstructions.fly(@image, env, "my-org", "my-group")

      assert step_two == """
             fly secrets set \\
               'MALICIOUS=x'\\'' ; touch /tmp/pwned ; echo '\\''' \\
               'MALICIOUS'\\'' ; touch /tmp/pwned ; '\\''=value'

             fly deploy --ha=false
             """
    end
  end

  describe "k8s/3" do
    test "quotes environment variable names and values" do
      env = [
        {"LIVEBOOK_AGENT_NAME", "default"},
        {"MALICIOUS\n  injected: true", "value\n  injected: true"}
      ]

      yaml = DeploymentGroupInstructions.k8s(@image, env, "my-group")

      assert yaml =~ """
                         - name: "LIVEBOOK_AGENT_NAME"
                           value: "default"\
             """

      assert yaml =~ """
                         - name: "MALICIOUS\\n  injected: true"
                           value: "value\\n  injected: true"\
             """
    end

    test "puts sensitive environment variables in a secret" do
      env = [
        {"LIVEBOOK_AGENT_NAME", "default"},
        {"LIVEBOOK_TEAMS_KEY", "lb_tk_key"}
      ]

      yaml = DeploymentGroupInstructions.k8s(@image, env, "my-group")

      assert yaml =~ """
                         - name: LIVEBOOK_TEAMS_KEY
                           valueFrom:
                             secretKeyRef:
                               name: livebook-secret
                               key: LIVEBOOK_TEAMS_KEY\
             """

      assert yaml =~ "  LIVEBOOK_TEAMS_KEY: #{Base.encode64("lb_tk_key")}"
    end
  end
end
