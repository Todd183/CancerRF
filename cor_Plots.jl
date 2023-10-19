
using Plots, DataFrames, CSV


cor_incidence_path = "data/correlation_result/cor_incidence.csv"

# Load data from data_path
cor_incidence = CSV.File(cor_incidence_path) |> DataFrame

scatter(cor_incidence.cor, -log10.(cor_incidence.p_adjusted), group=cor_incidence.cancer, xlabel="cor", ylabel="-log(p_adjusted)", title="Coefficient and P_value in Incidence",
markershape=:circle, markerstrokewidth=0, alpha=0.4, markerstrokealpha=0, size=(650, 400), legend=:outertopright)
hline!([2], color=:gray, linestyle=:dash, label="y=2")  
vline!([0.2, -0.2], color=:gray, linestyle=:dash, label="x=0.2, x=-0.2")  


