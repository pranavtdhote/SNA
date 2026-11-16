# =============================================================================
#  Coding Social Networks
#  Congress committee network built with igraph and visualised with ggraph
#
#  HOW TO RUN  (from the Practicals folder - the one that contains the .RData files)
#      Rscript 1/coding_social_networks.R
#  or open the file in RStudio, set the working directory to Practicals, and Source it.
#
#  Output : 1/output.txt   (everything printed below)
#           1/*.png        (every graph, also shown in the Plots pane in RStudio)
# =============================================================================

needed <- c("igraph", "ggraph", "ggplot2", "dplyr")
missing_pkgs <- needed[!vapply(needed, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  stop("Install the missing packages first:\n  install.packages(c(",
       paste0('"', missing_pkgs, '"', collapse = ", "), "))")
}
suppressPackageStartupMessages({
  library(igraph); library(ggraph); library(ggplot2); library(dplyr)
})

# ---- run-folder checks ------------------------------------------------------
out_dir <- "1"
if (!file.exists("congress.RData")) {
  stop("congress.RData not found in the current folder (", getwd(), ").\n",
       "Set the working directory to the Practicals folder and run again.")
}
dir.create(out_dir, showWarnings = FALSE)
options(width = 100, dplyr.summarise.inform = FALSE)

# ---- small printing helpers -------------------------------------------------
hr      <- function(ch = "=", n = 78) cat(strrep(ch, n), "\n", sep = "")
part    <- function(txt) { cat("\n"); hr(); cat(" ", txt, "\n", sep = ""); hr() }
kv      <- function(label, value) cat(sprintf("  %-52s %s\n", label, format(value, big.mark = ",")))
show_df <- function(d) print(as.data.frame(d), row.names = FALSE)
save_plot <- function(p, file, w = 10, h = 7) {
  ggsave(file.path(out_dir, file), p, width = w, height = h, dpi = 150, bg = "white")
  if (interactive()) print(p)
  cat("  [graph saved]", file.path(out_dir, file), "\n")
}
party_cols <- c(Democrat = "#2166ac", Republican = "#b2182b", Independent = "#4daf4a")

# One-row summary of a network, used to compare the networks side by side
network_summary <- function(g, label) {
  comp <- components(g)
  data.frame(
    Network      = label,
    Vertices     = vcount(g),
    Edges        = ecount(g),
    Density      = round(edge_density(g), 4),
    Avg_degree   = round(mean(degree(g)), 2),
    Max_degree   = max(degree(g)),
    Isolated     = sum(degree(g) == 0),
    Components   = comp$no,
    Largest_comp = max(comp$csize),
    Clustering   = round(transitivity(g, type = "global"), 4)
  )
}

sink(file.path(out_dir, "output.txt"), split = TRUE)
tryCatch({

  cat("Coding Social Networks\n")
  cat("Run on:", format(Sys.time(), "%d-%m-%Y %H:%M"), "\n")

  # ---------------------------------------------------------------------------
  #  Load data
  # ---------------------------------------------------------------------------
  suppressWarnings(load("congress.RData"))
  suppressWarnings(load("committees.RData"))

  part("DATA LOADED")
  kv("Members of Congress (congress)", nrow(congress))
  kv("  senators", sum(congress$type == "sen"))
  kv("  representatives", sum(congress$type == "rep"))
  kv("Committees (committees)", nrow(committees))
  kv("Committee memberships (committee_memberships)", nrow(committee_memberships))

  # vertices: one row per member, igraph needs the id column to be called "name"
  vertices_igraph <- congress %>%
    select(bioguide_id, full_name, type, party, gender) %>%
    distinct(bioguide_id, .keep_all = TRUE) %>%
    rename(name = bioguide_id)

  # membership table: (committee, member) pairs, only for members in `congress`
  memberships <- committee_memberships %>%
    inner_join(committees, by = "thomas_id") %>%
    select(thomas_id, bioguide_id) %>%
    distinct() %>%
    filter(bioguide_id %in% vertices_igraph$name)

  # common_committees: every pair of members + number of committees they share
  common_committees <- memberships %>%
    inner_join(memberships, by = "thomas_id",
               relationship = "many-to-many", suffix = c("_1", "_2")) %>%
    filter(bioguide_id_1 < bioguide_id_2) %>%
    group_by(bioguide_id_1, bioguide_id_2) %>%
    summarise(weight = n(), .groups = "drop")

  cat("\ncommon_committees (first 6 pairs):\n")
  show_df(head(common_committees, 6))

  # ===========================================================================
  #  A. igraph graph from common_committees
  # ===========================================================================
  part("A.  NETWORK OF ALL MEMBERS WHO SHARE AT LEAST ONE COMMITTEE")
  g <- graph_from_data_frame(common_committees, directed = FALSE, vertices = vertices_igraph)

  kv("Number of vertices", vcount(g))
  kv("Number of edges", ecount(g))
  kv("Network density", round(edge_density(g), 4))
  kv("Average degree", round(mean(degree(g)), 2))
  kv("Members with no committee partner (isolated)", sum(degree(g) == 0))

  cat("\nHow many committees do two connected members share?\n")
  weight_table <- common_committees %>%
    count(weight, name = "Pairs") %>%
    mutate(Share = paste0(round(100 * Pairs / sum(Pairs), 1), "%")) %>%
    rename(Shared_committees = weight)
  show_df(weight_table)

  cat("\nTop 10 most connected members (degree = number of committee partners):\n")
  top_members <- data.frame(
    Member = V(g)$full_name, Type = V(g)$type, Party = V(g)$party,
    Degree = degree(g), Strength = strength(g)
  ) %>% arrange(desc(Degree)) %>% head(10)
  show_df(top_members)

  p_weights <- ggplot(weight_table, aes(x = factor(Shared_committees), y = Pairs)) +
    geom_col(fill = "#2166ac") +
    geom_text(aes(label = format(Pairs, big.mark = ",")), vjust = -0.4, size = 3.4) +
    labs(title = "How many committees do connected members share?",
         x = "Number of shared committees", y = "Number of member pairs") +
    theme_minimal(base_size = 12)
  save_plot(p_weights, "shared_committees_distribution.png", 8, 5)

  # ===========================================================================
  #  B. Edges only between members on at least 3 committees together
  # ===========================================================================
  part("B.  NETWORK OF MEMBERS WHO SHARE AT LEAST 3 COMMITTEES")
  common_committees_3 <- common_committees %>% filter(weight >= 3)
  g3 <- graph_from_data_frame(common_committees_3, directed = FALSE, vertices = vertices_igraph)

  kv("Edges with weight >= 3 (answer)", ecount(g3))
  kv("Share of the original edges that survive",
     paste0(round(100 * ecount(g3) / ecount(g), 2), "%"))
  kv("Members that still have at least one tie", sum(degree(g3) > 0))
  kv("Members left without any tie", sum(degree(g3) == 0))

  cat("\nSide-by-side comparison:\n")
  show_df(rbind(network_summary(g, "A: >= 1 shared committee"),
                network_summary(g3, "B: >= 3 shared committees")))

  # ===========================================================================
  #  C. Visualisation with ggraph
  # ===========================================================================
  part("C.  VISUALISATION OF THE >= 3 COMMITTEES NETWORK (ggraph)")
  g3_plot <- induced_subgraph(g3, V(g3)[degree(g3) > 0])   # drop isolated dots
  V(g3_plot)$deg <- degree(g3_plot)
  cat("  Plotting", vcount(g3_plot), "connected members and", ecount(g3_plot), "ties.\n")
  set.seed(42)
  p_c <- ggraph(g3_plot, layout = "fr") +
    geom_edge_link(alpha = 0.25, colour = "grey45") +
    geom_node_point(aes(colour = party, shape = type, size = deg), alpha = 0.9) +
    scale_colour_manual(values = party_cols) +
    scale_shape_manual(values = c(sen = 17, rep = 16), labels = c(sen = "Senator", rep = "Representative")) +
    scale_size_continuous(range = c(1.5, 6), name = "Degree") +
    theme_void(base_size = 12) +
    labs(title = "Congress committee network",
         subtitle = "Members sharing at least 3 committees (colour = party, shape = chamber, size = degree)",
         colour = "Party", shape = "Chamber")
  save_plot(p_c, "committee_network_3plus.png", 11, 8)

  p_deg <- top_members %>%
    mutate(Member = factor(Member, levels = rev(Member))) %>%
    ggplot(aes(x = Member, y = Degree, fill = Party)) +
    geom_col() + coord_flip() +
    scale_fill_manual(values = party_cols) +
    labs(title = "Top 10 most connected members (all shared committees)",
         x = NULL, y = "Degree (number of committee partners)") +
    theme_minimal(base_size = 12)
  save_plot(p_deg, "top_connected_members.png", 8, 5)

  # ===========================================================================
  #  D. Network that includes only senators
  # ===========================================================================
  part("D.  SENATORS-ONLY NETWORK")
  senator_vertices <- vertices_igraph %>% filter(type == "sen")
  sen_ids <- senator_vertices$name

  senator_edges_all <- common_committees %>%
    filter(bioguide_id_1 %in% sen_ids, bioguide_id_2 %in% sen_ids)
  senator_edges <- senator_edges_all %>% filter(weight >= 3)

  g_sen_all <- graph_from_data_frame(senator_edges_all, directed = FALSE, vertices = senator_vertices)
  g_senators <- graph_from_data_frame(senator_edges, directed = FALSE, vertices = senator_vertices)

  kv("Senators (vertices)", vcount(g_senators))
  kv("Edges between senators (>= 1 shared committee)", ecount(g_sen_all))
  kv("Edges between senators (>= 3 shared committees)", ecount(g_senators))

  cat("\nSenator network summary:\n")
  show_df(rbind(network_summary(g_sen_all, "Senators, >= 1 committee"),
                network_summary(g_senators, "Senators, >= 3 committees")))

  party_idx <- as.integer(factor(V(g_senators)$party))
  kv("Party assortativity (+1 = ties only inside parties)",
     round(assortativity_nominal(g_senators, party_idx, directed = FALSE), 3))

  comm <- cluster_louvain(g_senators, weights = E(g_senators)$weight)
  kv("Communities found (Louvain)", length(comm))
  kv("Modularity of the community split", round(modularity(comm), 3))

  cat("\nTop 10 senators by degree in the >= 3 committees network:\n")
  V(g_senators)$deg <- degree(g_senators)
  top_sen <- data.frame(
    Senator = V(g_senators)$full_name, Party = V(g_senators)$party,
    State = congress$state[match(V(g_senators)$name, congress$bioguide_id)],
    Degree = degree(g_senators), Strength = strength(g_senators)
  ) %>% arrange(desc(Degree), desc(Strength)) %>% head(10)
  show_df(top_sen)

  label_cut <- sort(V(g_senators)$deg, decreasing = TRUE)[10]
  V(g_senators)$label <- ifelse(V(g_senators)$deg >= label_cut, V(g_senators)$full_name, NA)
  set.seed(42)
  p_d <- ggraph(g_senators, layout = "fr") +
    geom_edge_link(alpha = 0.3, colour = "grey45") +
    geom_node_point(aes(colour = party, size = deg)) +
    geom_node_text(aes(label = label), size = 3, vjust = -1, check_overlap = TRUE, na.rm = TRUE) +
    scale_colour_manual(values = party_cols) +
    scale_size_continuous(range = c(2, 7), name = "Degree") +
    theme_void(base_size = 12) +
    labs(title = "Senator committee network",
         subtitle = "Senators sharing at least 3 committees (best-connected senators are labelled)",
         colour = "Party")
  save_plot(p_d, "senator_network.png", 11, 8)

  # ---------------------------------------------------------------------------
  part("RESULT SUMMARY")
  kv("A. vertices / edges (>= 1 shared committee)", paste(vcount(g), "/", ecount(g)))
  kv("B. edges (>= 3 shared committees)", ecount(g3))
  kv("C. graphs written to folder", out_dir)
  kv("D. senators / edges (>= 3 shared committees)", paste(vcount(g_senators), "/", ecount(g_senators)))
  cat("\nCoding Social Networks - completed successfully.\n")

}, error = function(e) {
  cat("\nERROR:", conditionMessage(e), "\n")
  stop(e)
}, finally = {
  while (sink.number() > 0) sink()
})
