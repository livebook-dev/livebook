defmodule Livebook.K8sAPI.Kubeconfig do
  use Pluggable.StepBuilder

  step Kubereq.Kubeconfig.ENV, env_var: "KUBECONFIG"
  step Kubereq.Kubeconfig.ENV, env_var: "LIVEBOOK_KUBECONFIG"
  step Kubereq.Kubeconfig.ServiceAccount
end
