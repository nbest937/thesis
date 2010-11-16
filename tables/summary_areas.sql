create or replace view nbest.thesis_area_summary_v as
select mapset as group,
    substring(map from '^nlcd_([a-z]+)_a$') as cover,
    (sum /10^6)::numeric(5,1) as "Ma" 
from areas_global 
where map ~ '^nlcd' and map !~ 'dev|other|pasture'
union
select substring(map from '^2001_[a-z]+_(As0[05])_a$') as group, 
    substring(map from '^2001_([a-z]+)_') as cover,
    (sum /10^6)::numeric(5,1) as "Ma" 
from areas_global 
where map ~ '^2001_[a-z]+_(As0[05])_a$' and mask ~ 'lower48'
union
select 'agc' as group, 
    substring(map from '^agc_([a-z]+)_a$') as cover,
    (sum /10^6)::numeric(5,1) as "Ma" 
from areas_global
where map ~ 'agc'
union
select mapset as group, 
    case substring(map from '^([a-z]+)_a$') 
    when 'cropland' then 'crop'
    when 'pasture' then 'open'
    end  as cover,
    (sum /10^6)::numeric(5,1) as "Ma" 
from areas_global
where mapset = 'aglands' and map ~ '^(crop|past)';

drop view nbest.thesis_area_summary_ct;
create or replace view nbest.thesis_area_summary_ct as
select * from crosstab( $$
    select * 
    from nbest.thesis_area_summary_v 
    order by 1 $$, 
	$$
    select distinct cover 
    from nbest.thesis_area_summary_v 
    order by 1 $$)
as ct( "group" text, barren real, crop real, forest real, mosaic real, open real, shrub real, urban real, water real, wetland real);

drop view nbest.thesis_area_summary_ct2;
create or replace view nbest.thesis_area_summary_ct2 as
select * from crosstab( $$
    select cover, "group", "Ma"
    from (select * from nbest.thesis_area_summary_v 
	union select "group", 'total' as cover, sum("Ma") as "Ma"
	from nbest.thesis_area_summary_v 
	group by 1,2) as foo
    order by 1 $$, 
	$$
    select distinct "group" 
    from nbest.thesis_area_summary_v 
    order by 1 $$)
as ct( cover text, agc numeric(5,1), aglands numeric(5,1), As00 numeric(5,1), As05 numeric(5,1), nlcd numeric(5,1));
