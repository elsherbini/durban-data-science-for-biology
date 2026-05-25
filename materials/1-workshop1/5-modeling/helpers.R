entropy <- \(p) -sum(p * log(p))

fit_cytof <- function(formula, model_type = glm) {
    cell_types <- unique(reshaped_flow$cell_type)
    map(set_names(cell_types), \(ct) {
        flow2 <- flow |>
            mutate(
                total = rowSums(across(2:10)),
                proportion = .data[[ct]] / total
            )
        environment(formula) <- environment()
        model_type(formula, family = binomial, data = flow2, weights = total)
    })
}

simulate_counts <- function(models) {
    map_dfc(models, \(m) simulate(m)[["sim_1"]]) |>
        bind_cols(dplyr::select(flow, sample_id, arm)) |>
        pivot_longer(matches("cd|neutro"), names_to = "cell_type", values_to = "proportion")
}

plot_counts <- function(counts) {
    counts |>
        group_by(sample_id) |>
        mutate(proportion = proportion / sum(proportion)) |> # renormalize
        ggplot() +
        geom_col(aes(reorder(sample_id, proportion, entropy), proportion, fill = cell_type), col = "black", width = 1, linewidth = 0.3) +
        facet_wrap(~arm, scales = "free_x") +
        scale_y_continuous(expand = c(0, 0)) +
        labs(
            x = "Sample ID",
            fill = "Cell Type",
            y = "Proportion"
        ) +
        theme(axis.text.x = element_text(angle = 90))
}

simulate_cytokine <- function(fit, outcome = "IL-1a") {
    cytokines |>
        bind_cols(data.frame(simulated = exp(simulate(fit)$sim_1))) |>
        dplyr::select(cytokine, time_point, arm, pid, conc, simulated) |>
        rename(real = conc) |>
        pivot_longer(any_of(c("simulated", "real")))
}

plot_cytokine <- function(sim_data) {
    ggplot(sim_data) +
        geom_line(aes(time_point, log(value), group = pid, col = arm)) +
        scale_y_continuous(expand = c(0, 0), limits = c(0, 7)) +
        scale_x_discrete(expand = c(0, 0)) +
        facet_wrap(~name) +
        labs(
            x = "Time Point",
            color = "Arm",
            y = "log(concentration)"
        )
}
