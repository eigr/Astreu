defmodule AstreuK8s.Controller.V1.Astreu do
  @moduledoc """
  AstreuK8s: Astreu CRD.

  ## Kubernetes CRD Spec

  By default all CRD specs are assumed from the module name, you can override them using attributes.

  ### Examples
  ```
  # Kubernetes API version of this CRD, defaults to value in module name
  @version "v2alpha1"

  # Kubernetes API group of this CRD, defaults to "astreu-k8s.example.com"
  @group "kewl.example.io"

  The scope of the CRD. Defaults to `:namespaced`
  @scope :cluster

  CRD names used by kubectl and the kubernetes API
  @names %{
    plural: "foos",
    singular: "foo",
    kind: "Foo",
    shortNames: ["f", "fo"]
  }
  ```

  ## Declare RBAC permissions used by this module

  RBAC rules can be declared using `@rule` attribute and generated using `mix bonny.manifest`

  This `@rule` attribute is cumulative, and can be declared once for each Kubernetes API Group.

  ### Examples

  ```
  @rule {apiGroup, resources_list, verbs_list}

  @rule {"", ["pods", "secrets"], ["*"]}
  @rule {"apiextensions.k8s.io", ["foo"], ["*"]}
  ```

  ## Add additional printer columns

  Kubectl uses server-side printing. Columns can be declared using `@additional_printer_columns` and generated using `mix bonny.manifest`

  [Additional Printer Columns docs](https://kubernetes.io/docs/tasks/access-kubernetes-api/custom-resources/custom-resource-definitions/#additional-printer-columns)

  ### Examples
  
  ```
  @additional_printer_columns [
    %{
      name: "test",
      type: "string",
      description: "test",
      JSONPath: ".spec.test"
    }
  ]
  ```
  
  """
  use Bonny.Controller

  @group "astreu.eigr.io"
  @version "v1"

  @scope :namespaced
  @names %{
    plural: "astreus",
    singular: "astreu",
    kind: "Astreu",
    shortNames: []
  }

  @rule {"", ["pods", "configmap"], ["*"]}
  @rule {"", ["secrets"], ["create"]}
  @rule {"apps", ["statefulsets", "deployments"], ["*"]}

  @doc """
  Handles an `ADDED` event
  """
  @spec add(map()) :: :ok | :error
  @impl Bonny.Controller
  def add(%{} = astreu) do
    IO.inspect(astreu)
    :ok
  end

  @doc """
  Handles a `MODIFIED` event
  """
  @spec modify(map()) :: :ok | :error
  @impl Bonny.Controller
  def modify(%{} = astreu) do
    IO.inspect(astreu)
    :ok
  end

  @doc """
  Handles a `DELETED` event
  """
  @spec delete(map()) :: :ok | :error
  @impl Bonny.Controller
  def delete(%{} = astreu) do
    IO.inspect(astreu)
    :ok
  end

  @doc """
  Called periodically for each existing CustomResource to allow for reconciliation.
  """
  @spec reconcile(map()) :: :ok | :error
  @impl Bonny.Controller
  def reconcile(%{} = astreu) do
    IO.inspect(astreu)
    :ok
  end
end
