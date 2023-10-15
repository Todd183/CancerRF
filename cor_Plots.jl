using Plots
using CSV
using DataFrames

file_path = "D:/data422/data/correlation_result/cor_incidence.csv"
cor_incidence = CSV.File(file_path) |> DataFrame

# Display the first few rows of the DataFrame
display(cor_incidence)

# Assuming you have a DataFrame in Julia, and it's similar to the R data.frame
# For this example, let's create a DataFrame similar to your R code
cor_incidence = DataFrame(
    cor = randn(100),
    p_adjusted = rand(),
    cancer = rand(["A", "B"], 100)
)

# Julia equivalent code using Plots.jl
plot(
    cor_incidence[:cor], 
    -log10.(cor_incidence[:p_adjusted]),
    seriestype=:scatter,
    group=cor_incidence[:cancer],
    size=1.5,
    alpha=0.6,
    legend=:topleft,
    xlabel="cor",
    ylabel="-log10(p_adjusted)",
    color=:auto
)

vline!([-0.2, 0.2], linestyle=:dash, linecolor=:grey, linewidth=0.8)
hline!([2], linestyle=:dash, linecolor=:grey, linewidth=0.8)
