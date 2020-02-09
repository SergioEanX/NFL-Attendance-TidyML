NFL Attendance
================
2020-02-09

## Explore Data

``` r
attendance <-
    read_csv(
        "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-04/attendance.csv" , progress = TRUE
    )
standings <-
    read_csv(
        "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-04/standings.csv"
    )

attendance_joined <- attendance %>%
    dplyr::left_join(standings,
              by = c("year", "team_name", "team"))

attendance_joined$team_name <- factor(attendance_joined$team_name )
attendance_joined$team <- factor(attendance_joined$team)
attendance_joined$playoffs <- factor(attendance_joined$playoffs)
attendance_joined$sb_winner <- factor(attendance_joined$sb_winner)
```

``` r
attendance_joined %>% filter(!is.na(weekly_attendance)) %>%
    ggplot(aes(x=
    fct_reorder(team_name,weekly_attendance), 
    y=weekly_attendance,
    fill = playoffs), na.rm=TRUE) +
    geom_boxplot(outlier.alpha = 0.5) +
    coord_flip() + theme_minimal()
```

![](nfl_attendance_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

``` r
attendance_joined %>% distinct(team_name,
                               year, margin_of_victory,
                               playoffs) %>%
  ggplot(aes(margin_of_victory, fill=playoffs
             ))+
    geom_histogram(position = 'identity', alpha=0.7)
```

![](nfl_attendance_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
attendance_joined %>%
  mutate(week = factor(week)) %>%
  ggplot(aes(week, weekly_attendance, fill = week)) +
  geom_boxplot(show.legend = FALSE, outlier.alpha = 0.5) +
  labs(
    x = "Week of NFL season",
    y = "Weekly NFL game attendance"
  )+ theme_minimal()
```

![](nfl_attendance_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
attendance_df <- attendance_joined %>%
  filter(!is.na(weekly_attendance)) %>%
  select(
    weekly_attendance, team_name, year, week,
    margin_of_victory, strength_of_schedule, playoffs
  )

attendance_df
```

    ## # A tibble: 10,208 x 7
    ##    weekly_attendan… team_name  year  week margin_of_victo… strength_of_sch…
    ##               <dbl> <fct>     <dbl> <dbl>            <dbl>            <dbl>
    ##  1            77434 Cardinals  2000     1            -14.6             -0.7
    ##  2            66009 Cardinals  2000     2            -14.6             -0.7
    ##  3            71801 Cardinals  2000     4            -14.6             -0.7
    ##  4            66985 Cardinals  2000     5            -14.6             -0.7
    ##  5            44296 Cardinals  2000     6            -14.6             -0.7
    ##  6            38293 Cardinals  2000     7            -14.6             -0.7
    ##  7            62981 Cardinals  2000     8            -14.6             -0.7
    ##  8            35286 Cardinals  2000     9            -14.6             -0.7
    ##  9            52244 Cardinals  2000    10            -14.6             -0.7
    ## 10            64223 Cardinals  2000    11            -14.6             -0.7
    ## # … with 10,198 more rows, and 1 more variable: playoffs <fct>

## Train model

## Evaluate model
