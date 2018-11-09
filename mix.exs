defmodule KVX.Mixfile do
  use Mix.Project

  def project do
    [app: :kvs,
     version: "5.11.0",
     description: "Erlang Abstract Database",
     package: package]
  end

  defp package do
    [files: ~w(c_src include priv src LICENSE package.exs README.md rebar.config),
     licenses: ["MIT"],
     links: %{"GitHub" => "https://github.com/synrc/kvx"}]
   end
end
