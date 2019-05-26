data.fuel.orgin.Y18 <-subset(data.frame.validated, data.frame.validated$fuel_gov_origin == "YE18")
data.fuel.orgin.Y24 <-subset(data.frame.validated, data.frame.validated$fuel_gov_origin == "YE24")

data.WASH.orgin.Y18 <-subset(data.frame.validated, data.frame.validated$wash_gov_origin == "YE18")
data.WASH.orgin.Y24 <-subset(data.frame.validated, data.frame.validated$wash_gov_origin == "YE24")

median(data.fuel.orgin.Y18$calc_price_diesel,na.rm = T)
median(data.fuel.orgin.Y24$calc_price_diesel,na.rm = T)

median(data.fuel.orgin.Y18$calc_price_petrol,na.rm = T)
median(data.fuel.orgin.Y24$calc_price_petrol,na.rm = T)


median(data.WASH.orgin.Y18$calc_price_soap,na.rm=T)
median(data.WASH.orgin.Y24$calc_price_soap,na.rm=T)

median(data.WASH.orgin.Y18$calc_price_laundry,na.rm=T)
median(data.WASH.orgin.Y24$calc_price_laundry,na.rm=T)

median(data.WASH.orgin.Y18$calc_price_sanitary,na.rm=T)
median(data.WASH.orgin.Y24$calc_price_sanitary,na.rm=T)



data.fuel.origin<-subset(data.frame.validated, fuel_gov_origin == "YE18" | fuel_gov_origin == "YE24")

data.WASH.origin<-subset(data.frame.validated, wash_gov_origin == "YE18" | wash_gov_origin == "YE24")

aggregate(cbind(calc_price_petrol,calc_price_diesel)~ fuel_gov_origin, data = data.fuel.origin, FUN = median, na.rm=T)

aggregate(cbind(calc_price_soap,calc_price_laundry,calc_price_sanitary)~ wash_gov_origin, data = data.WASH.origin, FUN = median, na.rm=T)

item_origin_cost<-function(x=data.frame.validated){

data.fuel.origin<-subset(x, fuel_gov_origin == "YE18" | fuel_gov_origin == "YE24")

data.WASH.origin<-subset(x, wash_gov_origin == "YE18" | wash_gov_origin == "YE24")

fuel<-aggregate(cbind(calc_price_petrol,calc_price_diesel)~ fuel_gov_origin, data = data.fuel.origin, FUN = median, na.rm=T)

wash<-aggregate(cbind(calc_price_soap,calc_price_laundry,calc_price_sanitary)~ wash_gov_origin, data = data.WASH.origin, FUN = median, na.rm=T)


names(data.WASH.origin)[names(data.WASH.origin) == 'mrk_supply_issues/dmg_infra_market']<-'mrk_supply_issues_dmg_infra_market'
names(data.WASH.origin)[names(data.WASH.origin) == 'mrk_supply_issues/dmg_infra_sorrounding']<-'mrk_supply_issues_dmg_infra_sorrounding'
names(data.WASH.origin)[names(data.WASH.origin) == 'mrk_supply_issues/dmg_storage']<-'mrk_supply_issues_dmg_storage'
names(data.WASH.origin)[names(data.WASH.origin) == 'mrk_supply_issues/move_restriction']<-'mrk_supply_issues_move_restriction'
names(data.WASH.origin)[names(data.WASH.origin) == 'mrk_supply_issues/other']<-'mrk_supply_issues_other'
names(data.WASH.origin)[names(data.WASH.origin) == 'mrk_supply_issues/did_not_answer']<-'mrk_supply_issues_did_not_answer'
wash_restrict<-aggregate(cbind(mrk_supply_issues_dmg_infra_market,
                               mrk_supply_issues_dmg_infra_sorrounding,
                               mrk_supply_issues_dmg_storage,
                               mrk_supply_issues_move_restriction,
                               mrk_supply_issues_other)~ wash_gov_origin,
                                data=data.WASH.origin,FUN=sum, na.rm=T)

print(fuel)
print(wash)
print(wash_restrict)
}

