

plot(pitch_data_no_lg$ERA, pitch_data_no_lg$R)

plot(pitch_data_no_lg$CG, pitch_data_no_lg$R)

plot(pitch_data_no_lg$tSho, pitch_data_no_lg$R)

plot(pitch_data_no_lg$cSho, pitch_data_no_lg$R)

plot(pitch_data_no_lg$H, pitch_data_no_lg$R)

plot(pitch_data_no_lg$HR, pitch_data_no_lg$R)

plot(pitch_data_no_lg$BB, pitch_data_no_lg$R)

plot(pitch_data_no_lg$SO, pitch_data_no_lg$R)

plot(pitch_data_no_lg$HBP, pitch_data_no_lg$R)


scatter_func <- function(x.var, y.var){
    plot(x= x.var, y= y.var)
}

#Pitching to predict runs per game

scatter_func(join_data2$H9, join_data2$RA.G)

scatter_func(join_data2$HR9, join_data2$RA.G)

scatter_func(join_data2$BB9, join_data2$RA.G)

scatter_func(join_data2$FIP, join_data2$RA.G)

scatter_func(join_data2$WHIP, join_data2$RA.G)

scatter_func(join_data2$SO9, join_data2$RA.G)

scatter_func(join_data2$SO.W, join_data2$RA.G)

scatter_func(join_data2$LOB, join_data2$RA.G)

scatter_func(join_data2$ERAP, join_data2$RA.G)

scatter_func(join_data2$SV.G, join_data2$RA.G)

scatter_func(join_data2$CG.G, join_data2$RA.G)

scatter_func(join_data2$tSho.G, join_data2$RA.G)

scatter_func(join_data2$cSho.G, join_data2$RA.G)
