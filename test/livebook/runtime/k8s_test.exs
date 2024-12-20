defmodule Livebook.Runtime.K8sTest do
  use ExUnit.Case, async: true

  alias Livebook.Runtime

  # To run these tests, install [Kind](https://kind.sigs.k8s.io/) on
  # your machine. You can also set TEST_K8S_BUILD_IMAGE=1 to build
  # a container image, in case you make changes to start_runtime.exs.
  @moduletag :k8s

  @assert_receive_timeout 10_000
  @cluster_name "livebook-runtime-test"
  @kubeconfig_path "tmp/k8s_runtime/kubeconfig.yaml"

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

    System.put_env("KUBECONFIG", @kubeconfig_path)

    :ok
  end

  test "connecting flow" do
    config = config()

    assert [] = list_pods()

    pid = Runtime.K8s.new(config) |> Runtime.connect()

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

    assert [_] = list_pods()

    # Verify that we can actually evaluate code on the Kubernetes Pod
    Runtime.evaluate_code(runtime, :elixir, ~s/System.fetch_env!("POD_NAME")/, {:c1, :e1}, [])
    assert_receive {:runtime_evaluation_response, :e1, %{type: :terminal_text, text: text}, _meta}
    assert text =~ runtime.pod_name

    Runtime.disconnect(runtime)

    # Wait for Pod to terminate
    cmd!(~w(kubectl wait --for=jsonpath={.status.phase}=Succeeded pod/#{runtime.pod_name}))

    # Finally, delete the Pod object
    cmd!(~w(kubectl delete pod #{runtime.pod_name}))
  end

  defp config(attrs \\ %{}) do
    pod_template = """
    apiVersion: v1
    kind: Pod
    metadata:
      generateName: livebook-runtime-
      labels:
        livebook.dev/runtime: integration-test
    spec:
      containers:
        - name: livebook-runtime\
    """

    defaults = %{
      context: "kind-#{@cluster_name}",
      namespace: "default",
      pvc_name: nil,
      docker_tag: "nightly",
      pod_template: pod_template
    }

    Map.merge(defaults, attrs)
  end

  defp list_pods() do
    cmd!(
      ~w(kubectl get pod --selector=livebook.dev/runtime=integration-test --field-selector=status.phase==Running --output json)
    )
    |> JSON.decode!()
    |> Map.fetch!("items")
  end

  defp cmd!([command | args]) do
    {output, status} = System.cmd(command, args, stderr_to_stdout: true)

    if status != 0 do
      raise "command #{inspect(command)} #{inspect(args)} failed with output:\n#{output}"
    end

    output
  end
end
