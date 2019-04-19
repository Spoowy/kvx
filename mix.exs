defmodule KVX.Mixfile do
  use Mix.Project

  def project do
    [app: :kvx,
     version: "6.4.0",
     description: "Erlang Abstract Database",
     package: package()]
  end

  defp package do
    [files: ~w(include priv man etc src LICENSE mix.exs README.md rebar.config sys.config),
     licenses: ["MIT"],
     links: %{"GitHub" => "https://github.com/synrc/kvx"}]
   end
end
