
create table feil (
    stilling varchar primary key,
    man INT,
    tir int,
    ons INT,
    tor INT,
    fre INT,
    lor INT,
    son INT
);

create table logg (
    navn VARCHAR,
    stilling VARCHAR,
    man INT,
    tir INT,
    ons INT,
    tor INT,
    fre INT,
    lor INT,
    son INT
);

select cast(sum(s*man) as int) ||
       cast(sum(s*tir) as int) ||
       cast(sum(s*ons) as int) ||
       cast(sum(s*tor) as int) ||
       cast(sum(s*fre) as int) ||
       cast(sum(s*lor) as int) ||
       cast(sum(s*son) as int) 
  from (
select case when substr(navn,1,6)='Nissen'
       then 1
       else -1
       end as s,
       avg(man) as man,
       avg(tir) as tir,
       avg(ons) as ons,
       avg(tor) as tor,
       avg(fre) as fre,
       avg(lor) as lor,
       avg(son) as son
  from (select logg.navn,
               avg(max(logg.man-feil.man,0)) as man,
               avg(max(logg.tir-feil.tir,0)) as tir,
               avg(max(logg.ons-feil.ons,0)) as ons,
               avg(max(logg.tor-feil.tor,0)) as tor,
               avg(max(logg.fre-feil.fre,0)) as fre,
               avg(max(logg.lor-feil.lor,0)) as lor,
               avg(max(logg.son-feil.son,0)) as son
          from feil,logg where feil.stilling=logg.stilling
        group by logg.navn)
group by s
  );


update logg set stilling=trim(stilling),navn=trim(navn);
update feil set stilling=trim(stilling);
delete from logg where navn like '##%';