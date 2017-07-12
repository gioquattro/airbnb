by_iso_guest_host = as.data.table(
  sqldf("select t1.Code3 guest_iso, t2.Code3 host_iso
         from review_iso t1
         join listing_iso t2 on t1.listing_id=t2.id")
)

by_iso_guest_host$cnt = 1
by_iso_guest_host_g = by_iso_guest_host[, .(n=length(cnt)), by=list(guest_iso,host_iso)]

names(by_iso_guest_host_g)
by_guest = by_iso_guest_host_g[, .(cnt_g=sum(n)), by=guest_iso]
by_host = by_iso_guest_host_g[, .(cnt_h=sum(n)), by=host_iso]

setkey(by_iso_guest_host_g, guest_iso)
setkey(by_guest, guest_iso)
by_iso_guest_host_g = by_iso_guest_host_g[by_guest, nomatch=0]

setkey(by_iso_guest_host_g, host_iso)
setkey(by_host, host_iso)
by_iso_guest_host_g = by_iso_guest_host_g[by_host, nomatch=0]

by_iso_guest_host_g$rg = by_iso_guest_host_g$n / by_iso_guest_host_g$cnt_g
by_iso_guest_host_g$rh = by_iso_guest_host_g$n / by_iso_guest_host_g$cnt_h
by_iso_guest_host_g = by_iso_guest_host_g[n>100]

names(by_iso_guest_host_g)
by_iso_guest_host_g = by_iso_guest_host_g[guest_iso != host_iso]

