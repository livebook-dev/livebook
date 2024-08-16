defmodule Livebook.K8s.Kubeconfig do
  use Pluggable.StepBuilder

  step Kubereq.Kubeconfig.ENV, env_var: "KUBECONFIG"
  step Kubereq.Kubeconfig.ENV, env_var: "LIVEBOOK_KUBECONFIG"
  step Kubereq.Kubeconfig.ServiceAccount
end
