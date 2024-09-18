defmodule Livebook.Runtime.K8sTest do
  alias Livebook.Runtime
  use ExUnit.Case, async: true

  # To run these tests, install [Kind](https://kind.sigs.k8s.io/) on your machine.
  @moduletag :k8s

  @assert_receive_timeout 10_000
  @cluster_name "livebook-runtime-test"
  @kubeconfig_path "tmp/k8s_runtime/kubeconfig.yaml"

  @default_pod_template """
  apiVersion: v1
  kind: Pod
  metadata:
    generateName: livebook-runtime-
    labels:
      livebook.dev/runtime: integration-test
  spec:
    containers:
      - image: ghcr.io/livebook-dev/livebook:nightly
        name: livebook-runtime
        env:
          - name: TEST_VAR
            value: present

  """
  setup_all do
    unless System.find_executable("kind") do
      raise "kind is not installed"
    end

    clusters = cmd!(~w(kind get clusters)) |> String.split("\n", trim: true)

    if @cluster_name not in clusters do
      cmd!(~w(kind create cluster --name #{@cluster_name}))
    end

    # Export kubeconfig file
    cmd!(~w(kind export kubeconfig --name #{@cluster_name} --kubeconfig #{@kubeconfig_path}))

    # In most cases we can use the existing image, but when making
    # changes to the remote runtime code, we need to build a new image
    if System.get_env("TEST_K8S_BUILD_IMAGE") in ~w(true 1) do
      {_, versions} = Code.eval_file("versions")

      cmd!(~w(docker build
          --build-arg BASE_IMAGE=hexpm/elixir:#{versions[:elixir]}-erlang-#{versions[:otp]}-ubuntu-#{versions[:ubuntu]}
          --build-arg VARIANT=default
          -t ghcr.io/livebook-dev/livebook:nightly .))
    else
      cmd!(~w(docker image pull ghcr.io/livebook-dev/livebook:nightly))
    end

    # Load container image into Kind cluster
    cmd!(~w(kind load docker-image --name #{@cluster_name} ghcr.io/livebook-dev/livebook:nightly))

    :ok
  end

  test "connecting flow" do
    config = config()
    req = req()

    assert [] = list_pods(req)

    pid = Runtime.K8s.new(config, req) |> Runtime.connect()

    assert_receive {:runtime_connect_info, ^pid, "create pod"}, @assert_receive_timeout

    assert_receive {:runtime_connect_info, ^pid, "waiting for pod"}, @assert_receive_timeout

    assert_receive {:runtime_connect_info, ^pid, "created container livebook-runtime"},
                   @assert_receive_timeout

    assert_receive {:runtime_connect_info, ^pid, "started container livebook-runtime"},
                   @assert_receive_timeout

    assert_receive {:runtime_connect_info, ^pid, "start proxy"}, @assert_receive_timeout
    assert_receive {:runtime_connect_info, ^pid, "connect to node"}, @assert_receive_timeout
    assert_receive {:runtime_connect_info, ^pid, "initialize node"}, @assert_receive_timeout
    assert_receive {:runtime_connect_done, ^pid, {:ok, runtime}}, @assert_receive_timeout

    Runtime.take_ownership(runtime)

    assert [_] = list_pods(req)

    # Verify that we can actually evaluate code on the Kubernetes Pod
    Runtime.evaluate_code(runtime, :elixir, ~s/System.fetch_env!("TEST_VAR")/, {:c1, :e1}, [])
    assert_receive {:runtime_evaluation_response, :e1, %{type: :terminal_text, text: text}, _meta}
    assert text =~ "present"

    Runtime.disconnect(runtime)

    # Wait for Pod to terminate
    assert :ok ==
             Kubereq.wait_until(
               req,
               "default",
               runtime.pod_name,
               &(&1["status"]["phase"] == "Succeeded")
             )

    # Finally, delete the Pod object
    Kubereq.delete(req, "default", runtime.pod_name)
  end

  defp req() do
    [Kubereq.Kubeconfig.ENV, {Kubereq.Kubeconfig.File, path: @kubeconfig_path}]
    |> Kubereq.Kubeconfig.load()
    |> Kubereq.new("api/v1/namespaces/:namespace/pods/:name")
  end

  defp config(attrs \\ %{}) do
    defaults = %{
      context: "kind-#{@cluster_name}",
      namespace: "default",
      home_pvc: nil,
      docker_tag: "nightly",
      pod_template: @default_pod_template
    }

    Map.merge(defaults, attrs)
  end

  defp list_pods(req) do
    {:ok, resp} =
      Kubereq.list(req, "default",
        label_selectors: [{"livebook.dev/runtime", "integration-test"}],
        field_selectors: [{"status.phase", "Running"}]
      )

    resp.body["items"]
  end

  defp cmd!([command | args]) do
    {output, status} = System.cmd(command, args, stderr_to_stdout: true)

    if status != 0 do
      raise "command #{inspect(command)} #{inspect(args)} failed"
    end

    output
  end
end
