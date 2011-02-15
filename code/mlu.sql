select id_1, name_1 from gadm.v1_lev1 where iso='USA';

select * from (select id_1 as gadm1, name_1 as state from gadm.v1_lev1 where iso='USA'and id_1 not in (3192, 3199, 3202)) as g natural join (select state, use, k_acres from usda.mlu where year = 2002) as m;

create or replace view gadm.cusa as
select ogc_fid, wkb_geometry, id_1, name_1
from gadm.v1_lev1
where iso='USA'and id_1 not in (3192, 3199, 3202);

insert into geometry_columns values ('', 'gadm', 'cusa', 'wkb_geometry', 2, 4326, 'MULTIPOLYGON');

