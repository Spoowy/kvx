defmodule KVX.Mixfile do
  use Mix.Project

  def project do
    [app: :kvx,
     version: "6.4.0",
     description: "Abstract Chain Database",
     package: package(),
     deps: deps()]
  end

  defp package do
    [files: ~w(include man etc src LICENSE mix.exs README.md rebar.config sys.config),
     licenses: ["MIT"],
     links: %{"GitHub" => "https://github.com/synrc/kvx"}]
   end

   defp deps do
     [{:ex_doc, ">= 0.0.0", only: :dev}]
   end

end
