)library(readr)
library(choroplethr)
library(choroplethrMaps)
library(ggplot2)
library(ggthemes)
library(RColorBrewer)
library(gridExtra)
library(dplyr)
library(stringr)
library(tidyr)

all_results <- read_csv("https://raw.githubusercontent.com/AlexTRee/USELECTION2016/master/presidential_election_county_results_2016.csv")

ca <- all_results %>% 
	 filter(str_detect(fips, "^06"))

ca_spread <- ca %>% 
     filter(cand_name %in% c("Donald Trump", "Hillary Clinton", "Gary Johnson")) %>%
     select(fips, cand_name, pct) %>% 
     mutate(pct = pct * 100, region = as.numeric(fips)) %>%
     spread(cand_name, pct) %>%
     select(-fips)

ca_spread$value <- ca_spread$`Donald Trump`
choro1 = CountyChoropleth$new(ca_spread)
choro1$set_zoom("california")
choro1$title = "Donald Trump"
choro1$set_num_colors(1)
choro1$ggplot_polygon = geom_polygon(aes(fill = value), color = NA)
choro1$ggplot_scale = scale_fill_gradientn(name = "Percent", colours = brewer.pal(8, "Reds"))

ca_spread$value <- ca_spread$`Hillary Clinton`
choro2 = CountyChoropleth$new(ca_spread)
choro2$set_zoom("california")
choro2$title = "Hillary Clinton"
choro2$set_num_colors(1)
choro2$ggplot_polygon = geom_polygon(aes(fill = value), color = NA)
choro2$ggplot_scale = scale_fill_gradientn(name = "Percent", colours = brewer.pal(8, "Blues"))

ca_spread$value <- ca_spread$`Gary Johnson`
choro3 = CountyChoropleth$new(ca_spread)
choro3$set_zoom("california")
choro3$title = "Gary Johnson"
choro3$set_num_colors(1)
choro3$ggplot_polygon = geom_polygon(aes(fill = value), color = NA)
choro3$ggplot_scale = scale_fill_gradientn(name = "Percent", colours = brewer.pal(8, "Greens"))
										  

										  
md <- all_results %>% 
     filter(str_detect(fips, "^24"))										  

md_spread <- md %>% 
     filter(cand_name %in% c("Donald Trump", "Hillary Clinton", "Gary Johnson")) %>%
     select(fips, cand_name, pct) %>% 
     mutate(pct = pct * 100, region = as.numeric(fips)) %>%
     spread(cand_name, pct) %>%
     select(-fips)
	 
md_spread$value <- md_spread$`Donald Trump`
choro4 = CountyChoropleth$new(md_spread)
choro4$set_zoom("maryland")
choro4$title = "Donald Trump"
choro4$set_num_colors(1)
choro4$ggplot_polygon = geom_polygon(aes(fill = value), color = NA)
choro4$ggplot_scale = scale_fill_gradientn(name = "Percent", colours = brewer.pal(8, "Reds"))
										   
md_spread$value <- md_spread$`Hillary Clinton`
choro5 = CountyChoropleth$new(md_spread)
choro5$set_zoom("maryland")
choro5$title = "Hillary Clinton"
choro5$set_num_colors(1)
choro5$ggplot_polygon = geom_polygon(aes(fill = value), color = NA)
choro5$ggplot_scale = scale_fill_gradientn(name = "Percent", colours = brewer.pal(8, "Blues"))

md_spread$value <- md_spread$`Gary Johnson`
choro6 = CountyChoropleth$new(md_spread)
choro6$set_zoom("maryland")
choro6$title = "Gary Johnson"
choro6$set_num_colors(1)
choro6$ggplot_polygon = geom_polygon(aes(fill = value), color = NA)
choro6$ggplot_scale = scale_fill_gradientn(name = "Percent", colours = brewer.pal(8, "Greens"))


										   
va <- all_results %>% 
     filter(str_detect(fips, "^51"))

va_spread <- va %>% 
     filter(cand_name %in% c("Donald Trump", "Hillary Clinton", "Gary Johnson")) %>%
     select(fips, cand_name, pct) %>% 
     mutate(pct = pct * 100, region = as.numeric(fips)) %>%
     spread(cand_name, pct) %>%
     select(-fips)

va_spread$value <- va_spread$`Donald Trump`
choro7 = CountyChoropleth$new(va_spread)
choro7$set_zoom("virginia")
choro7$title = "Donald Trump"
choro7$set_num_colors(1)
choro7$ggplot_polygon = geom_polygon(aes(fill = value), color = NA)
choro7$ggplot_scale = scale_fill_gradientn(name = "Percent", colours = brewer.pal(8, "Reds"))

va_spread$value <- va_spread$`Hillary Clinton`
choro8 = CountyChoropleth$new(va_spread)
choro8$set_zoom("virginia")
choro8$title = "Hillary Clinton"
choro8$set_num_colors(1)
choro8$ggplot_polygon = geom_polygon(aes(fill = value), color = NA)
choro8$ggplot_scale = scale_fill_gradientn(name = "Percent", colours = brewer.pal(8, "Blues"))

va_spread$value <- va_spread$`Gary Johnson`
choro9 = CountyChoropleth$new(va_spread)
choro9$set_zoom("virginia")
choro9$title = "Gary Johnson"
choro9$set_num_colors(1)
choro9$ggplot_polygon = geom_polygon(aes(fill = value), color = NA)
choro9$ggplot_scale = scale_fill_gradientn(name = "Percent", colours = brewer.pal(8, "Greens"))



grid.arrange(choro1$render() + theme(text=element_text(family="KerkisSans")), 
              choro2$render() + theme(text=element_text(family="KerkisSans")), 
              choro3$render() + theme(text=element_text(family="KerkisSans")),
              choro4$render() + theme(text=element_text(family="KerkisSans")), 
              choro5$render() + theme(text=element_text(family="KerkisSans")), 
              choro6$render() + theme(text=element_text(family="KerkisSans")), 
              choro7$render() + theme(text=element_text(family="KerkisSans")), 
              choro8$render() + theme(text=element_text(family="KerkisSans")), 
              choro9$render() + theme(text=element_text(family="KerkisSans")), ncol = 3, top ="2016 US Election Choropleth by Tiange Cui", left = "Virginia/Maryland/California")
