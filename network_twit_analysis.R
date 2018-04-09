rt <- search_tweets(
  "#mpsa18", n = 18000, include_rts = FALSE
)

unlist_df <- function(...) {
  dots <- lapply(list(...), unlist)
  tibble::as_tibble(dots)
}

## iterate by row
row_dfs <- lapply(
  seq_len(nrow(rt)), function(i)
    unlist_df(from_screen_name = rt$screen_name[i],
              reply = rt$reply_to_screen_name[i],
              mention = rt$mentions_screen_name[i],
              quote = rt$quoted_screen_name[i],
              retweet = rt$retweet_screen_name[i])
)


rdf <- dplyr::bind_rows(row_dfs)
rdf <- tidyr::gather(rdf, interaction_type, to_screen_name, -from_screen_name)
mat <- as.matrix(rdf[, -2])
mat <- mat[apply(mat, 1, function(i) !any(is.na(i))), ]

## get rid of self references
mat <- mat[mat[, 1] != mat[, 2], ]

## filter out users who don't appear in RHS at least 3 times
apps1 <- table(mat[, 1])
apps1 <- apps1[apps1 > 1L]
apps2 <- table(mat[, 2])
apps2 <- apps2[apps2 > 1L]
apps <- names(apps1)[names(apps1) %in% names(apps2)]
mat <- mat[mat[, 1] %in% apps & mat[, 2] %in% apps, ]

g <- igraph::graph_from_edgelist(mat)

## calculate size attribute (and transform to fit)
matcols <- factor(c(mat[, 1], mat[, 2]), levels = names(igraph::V(g)))
size <- table(screen_name = matcols)
size <- (log(size) + sqrt(size)) / 3

## reorder freq table
size <- size[match(names(size), names(igraph::V(g)))]

## plot and save network image
png("mpsa.png", width = 32, height = 32, units = "in", res = 300)
par(mar = c(12, 6, 15, 6), bg = "#F6F8FA")
plot(g,
     edge.size = .4,
     curved = FALSE,
     margin = -.05,
     edge.arrow.size = 0,
     edge.arrow.width = 0,
     vertex.color = "#ADFF2F99",
     vertex.size = size,
     vertex.frame.color = "#003366",
     vertex.label.color = "#003366",
     vertex.label.cex = .8,
     vertex.label.family = "Product Sans",
     edge.color = "#0066aa",
     edge.width = .2,
     main = "")
par(mar = c(4, 6, 8, 6))
title("Semantic network of users tweeting about #NICAR18",
      adj = 0, family = "Product Sans", cex.main = 6.5)
mtext("Source: Data gathered using rtweet. Network analysis done using igraph",
      side = 1, line = 1, adj = 1.0, cex = 3.8,
      family = "Product Sans", col = "#222222")
mtext("User connections by mentions, replies, retweets, and quotes",
      side = 3, line = -4.25, adj = 0,
      family = "Product Sans", cex = 4.9)
dev.off()


ggraph(routes_igraph_tidy) + 
  geom_edge_link(alpha = 0.1, colour = "cyan4") + 
  geom_node_point(colour = "darkorchid", size = size, alpha = .45) + 
  geom_node_text(aes(label = name), size = 2.5, family = "Product Sans", repel = TRUE) + 
  theme_graph() +
  labs(title = "Network Analysis of #MPSA18", subtitle = "Connections Based on Mentions, Retweets, Quotes, and Replies", caption = "Code Borrowed from @kearneymw") +
  theme(plot.title = element_text(family = "Product Sans", size = 54, vjust =2, face = "bold")) +
  theme(plot.subtitle = element_text(family = "Product Sans", size = 20, vjust =-1)) +
  theme(plot.caption = element_text(family = "Product Sans", size =20))

ggsave(file="D://rtweets/mpsa_final3.png", type = "cairo-png", width = 20, height =12)

