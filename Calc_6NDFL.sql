CREATE PROCEDURE [dbo].[SPCALC_NDFL6]
	@Id int,
	@IncludeLastPrevMonth smallint = 0
AS

declare @IdUch	smallint
declare @CMonth smallint
declare @Month  smallint
declare @CYear	smallint
declare @LastMonth smallint
declare @Year	smallint

select @IdUch = dbo.CurrentIdUch()
select @CYear = PeriodYear-1900, @CMonth = 3*PeriodQuarter, @LastMonth = 3*PeriodQuarter from TNDFL6_Header where [Id] = @Id
select @Month = dbo.CMonth(@IdUch)
select @Year = dbo.CYear(@IdUch)
if (@Month < @CMonth and @Year = @CYear) select @CMonth = @Month

update TNDFL6_Header
set
	Podpisant = VA1NAST.Подписант,
	KodNalog = VA1NAST.НалоговаяИнспекция,
	Korrektirovka = 0,
	DocumentListsCount = 0,
	PodpisantType = 1,
	KodLocation = 212
from TNDFL6_Header, VA1NAST
where TNDFL6_Header.[Id] = @Id

create table #ZPNL(
	ТабельныйНомер	smallint,
	Льгота		numeric(18,2),
	Совокупный	numeric(18,2),
	Облагается	numeric(18,2),
	Расчитано	numeric(18,2),
	Подоходный	numeric(18,2),
	Доплата		numeric(18,2),
	Матпомощь	numeric(18,2),
	МатпомощьСкидка	numeric(18,2),
	Нерезидент	smallint,
	М1		smallint, Д1 numeric(18,2), КД1 varchar(4), ДВ1 numeric(18,2), ДКВ1 varchar(4),
	М2 smallint, Д2 numeric(18,2), КД2 varchar(4), ДВ2 numeric(18,2), ДКВ2 varchar(4),
	М3 smallint, Д3 numeric(18,2), КД3 varchar(4), ДВ3 numeric(18,2), ДКВ3 varchar(4),
	М4 smallint, Д4 numeric(18,2), КД4 varchar(4), ДВ4 numeric(18,2), ДКВ4 varchar(4),
	М5 smallint, Д5 numeric(18,2), КД5 varchar(4), ДВ5 numeric(18,2), ДКВ5 varchar(4),
	М6 smallint, Д6 numeric(18,2), КД6 varchar(4), ДВ6 numeric(18,2), ДКВ6 varchar(4),
	М7 smallint, Д7 numeric(18,2), КД7 varchar(4), ДВ7 numeric(18,2), ДКВ7 varchar(4),
	М8 smallint, Д8 numeric(18,2), КД8 varchar(4), ДВ8 numeric(18,2), ДКВ8 varchar(4),
	М9 smallint, Д9 numeric(18,2), КД9 varchar(4), ДВ9 numeric(18,2), ДКВ9 varchar(4),
	М10 smallint, Д10 numeric(18,2), КД10 varchar(4), ДВ10 numeric(18,2), ДКВ10 varchar(4),
	М11 smallint, Д11 numeric(18,2), КД11 varchar(4), ДВ11 numeric(18,2), ДКВ11 varchar(4),
	М12 smallint, Д12 numeric(18,2), КД12 varchar(4), ДВ12 numeric(18,2), ДКВ12 varchar(4),
	М13 smallint, Д13 numeric(18,2), КД13 varchar(4), ДВ13 numeric(18,2), ДКВ13 varchar(4),
	М14 smallint, Д14 numeric(18,2), КД14 varchar(4), ДВ14 numeric(18,2), ДКВ14 varchar(4),
	М15 smallint, Д15 numeric(18,2), КД15 varchar(4), ДВ15 numeric(18,2), ДКВ15 varchar(4),
	М16 smallint, Д16 numeric(18,2), КД16 varchar(4), ДВ16 numeric(18,2), ДКВ16 varchar(4),
	М17 smallint, Д17 numeric(18,2), КД17 varchar(4), ДВ17 numeric(18,2), ДКВ17 varchar(4),
	М18 smallint, Д18 numeric(18,2), КД18 varchar(4), ДВ18 numeric(18,2), ДКВ18 varchar(4),
	М19 smallint, Д19 numeric(18,2), КД19 varchar(4), ДВ19 numeric(18,2), ДКВ19 varchar(4),
	М20 smallint, Д20 numeric(18,2), КД20 varchar(4), ДВ20 numeric(18,2), ДКВ20 varchar(4),
	М21 smallint, Д21 numeric(18,2), КД21 varchar(4), ДВ21 numeric(18,2), ДКВ21 varchar(4),
	М22 smallint, Д22 numeric(18,2), КД22 varchar(4), ДВ22 numeric(18,2), ДКВ22 varchar(4),
	М23 smallint, Д23 numeric(18,2), КД23 varchar(4), ДВ23 numeric(18,2), ДКВ23 varchar(4),
	М24 smallint, Д24 numeric(18,2), КД24 varchar(4), ДВ24 numeric(18,2), ДКВ24 varchar(4),
	М25 smallint, Д25 numeric(18,2), КД25 varchar(4), ДВ25 numeric(18,2), ДКВ25 varchar(4),
	М26 smallint, Д26 numeric(18,2), КД26 varchar(4), ДВ26 numeric(18,2), ДКВ26 varchar(4),
	М27 smallint, Д27 numeric(18,2), КД27 varchar(4), ДВ27 numeric(18,2), ДКВ27 varchar(4),
	М28 smallint, Д28 numeric(18,2), КД28 varchar(4), ДВ28 numeric(18,2), ДКВ28 varchar(4),
	Кв1 smallint, Св1 numeric(18,2), Кв2 smallint, Св2 numeric(18,2), Кв3 smallint,
	Св3 numeric(18,2), Кв4 smallint, Св4 numeric(18,2), Кв5 smallint, Св5 numeric(18,2),
	Кв6 smallint, Св6 numeric(18,2), Кв7 smallint, Св7 numeric(18,2), Кв8 smallint, Св8 numeric(18,2)
)

create table #ZPNPATENT (
	[Id]		smallint IDENTITY(1,1),
	TN			smallint,
	PatentSumma numeric(18, 2),
	KodRezidenta smallint
)

create table #ZPNMES(
	ТабельныйНомер		smallint,
	Месяц			smallint,
	Период			smallint,
	Совокупный		numeric(18,2),
	СовокупныйМ		numeric(18,2),
	Матпомощь		numeric(18,2),
	КодВычета		smallint,
	Минимумы		numeric(18,2),
	КодВычетаИждивенцев	smallint,
	Иждивенцы		numeric(18,2),
	[Облагаемый]		numeric(18,2),
	[Подоходный]		numeric(18,2),
	[KИждивенцы]		smallint
)

create table #ZPNMESDVich(
	ТабельныйНомер	smallint,
	Месяц		smallint,
	КодВычета	smallint,
	СуммаВычета	numeric(18,2)
)

create table #ZPNMESD(
	ТабельныйНомер	smallint,
	Месяц		smallint,
	КодДохода	smallint,
	СуммаДохода	numeric(18,2),
	КодВычета	smallint,
	СуммаВычета	numeric(18,2)
)

create table #ZPNMESD_t(
	ТабельныйНомер	smallint,
	Месяц		smallint,
	КодДохода	smallint,
	СуммаДохода	numeric(18,2),
	КодВычета	smallint,
	СуммаВычета	numeric(18,2)
)

create table #ZPNMESV(
	ТабельныйНомер	smallint,
	КодВычета	smallint,
	СуммаВычета	numeric(18,2)
)

create table #Tdoh(
	Ид		smallint IDENTITY(1,1),
	ТабельныйНомер	smallint,
	Уровень		smallint,
	М		smallint,
	КД		varchar(4),
	Д		numeric(18,2),
	ДВ		numeric(18,2),
	ДКВ		varchar(4)
)

create table #Tv(
	Ид		smallint IDENTITY(1,1),
	ТабельныйНомер	smallint,
	Уровень		smallint,
	Кв		smallint,
	Св		numeric(18,2)
)

create table #Ttn (ТабельныйНомер smallint)
insert into #Ttn 
select ТабельныйНомер
from VSELECT_LS where VSELECT_LS.Ргод = @CYear and VSELECT_LS.ИдУч = @IdUch

exec SPCALC_NDFL @IdUch, @CYear, @CMonth, 3
--delete from #ZPNL where (isnull(Совокупный, 0) = 0 and isnull(Облагается, 0) = 0 and isnull(Подоходный, 0) = 0)

-- /////////// Вычеты //////////////////////////////////////////////////////////
-- /////////// Вычеты //////////////////////////////////////////////////////////
-- /////////// Вычеты //////////////////////////////////////////////////////////

update #ZPNMESDVich
set КодВычета = convert(smallint, left(convert(varchar(10), КодВычета), 3))
where КодВычета > 999 and КодВычета < 2000

insert into #ZPNMESD_t 
select	ТабельныйНомер, Месяц, КодДохода, sum(СуммаДохода) as СуммаДохода, КодВычета, isnull(sum(СуммаВычета), 0) as СуммаВычета
from	#ZPNMESD
group by ТабельныйНомер, Месяц, КодДохода,КодВычета
having sum(СуммаДохода) > 0 or (sum(СуммаВычета) > 0 and КодВычета between 300 and 330)
order by ТабельныйНомер, Месяц, КодДохода

delete from #ZPNMESD
insert into #ZPNMESD
select * from #ZPNMESD_t

insert into #ZPNMESV (ТабельныйНомер, КодВычета, СуммаВычета)
select ТабельныйНомер, КодВычета, isnull(sum(СуммаВычета), 0) as СуммаВычета
from #ZPNMESDVich
where #ZPNMESDVich.Месяц <= @CMonth group by ТабельныйНомер, КодВычета

DECLARE CTN CURSOR
READ_ONLY
FOR select ТабельныйНомер, Совокупный FROM #ZPNL where Облагается = 0 

DECLARE @TN smallint, @SumDoh numeric(18,2), @SumIspVichAll numeric(18,2), @SumIspVich numeric(18,2)
OPEN CTN

FETCH NEXT FROM CTN INTO @TN,@SumDoh
WHILE (@@fetch_status <> -1)
BEGIN
	IF (@@fetch_status <> -2)
	BEGIN
		select @SumIspVich = 0,@SumIspVichAll = 0

		update #ZPNMESV
		set @SumIspVich =
			case 
				when @SumDoh >= СуммаВычета + @SumIspVichAll  then СуммаВычета 
				when @SumDoh < @SumIspVichAll then 0
				when @SumDoh < СуммаВычета + @SumIspVichAll  then @SumDoh - @SumIspVichAll
			end,
			@SumIspVichAll = @SumIspVichAll + @SumIspVich,
			СуммаВычета = @SumIspVich
		from #ZPNMESV
		where ТабельныйНомер = @TN and КодВычета not between 300 and 400
	END
	delete from #ZPNMESV where СуммаВычета = 0 and ТабельныйНомер = @TN
FETCH NEXT FROM CTN INTO @TN,@SumDoh
END

CLOSE CTN
DEALLOCATE CTN

delete #ZPNMESV where isnull(СуммаВычета,0) = 0

/*
select sum(СуммаВычета) from #ZPNMESV --where Месяц <= 2
--inner join #Ttn on #Ttn.ТабельныйНомер = #ZPNMESV.ТабельныйНомер
where КодВычета  not between 300 and 320
*/

update TNDFL6_Header
set SummaVichet13 = sv
from (
	select sum(СуммаВычета) sv
	from #ZPNL
	inner join #Ttn on #Ttn.ТабельныйНомер = #ZPNL.ТабельныйНомер
	left outer join #ZPNMESV on #ZPNMESV.ТабельныйНомер = #ZPNL.ТабельныйНомер
	where isnull(#ZPNL.Нерезидент, 0) = 0
	) t
where TNDFL6_Header.[Id] = @Id

update TNDFL6_Header
set SummaVichet30 = sv
from (
	select sum(СуммаВычета) sv
	from #ZPNL
	inner join #Ttn on #Ttn.ТабельныйНомер = #ZPNL.ТабельныйНомер
	left outer join #ZPNMESV on #ZPNMESV.ТабельныйНомер = #ZPNL.ТабельныйНомер
	where isnull(#ZPNL.Нерезидент, 0) <> 0
	) t
where TNDFL6_Header.[Id] = @Id

----------- остальное

insert into #Tdoh (ТабельныйНомер, М, КД, Д,ДВ,ДКВ)
select	ТабельныйНомер, Месяц, КодДохода, sum(СуммаДохода) as СуммаДохода,sum(СуммаВычета) as СуммаВычета,КодВычета
from	#ZPNMESD --where isnull(СуммаДохода,0) > 0
group by ТабельныйНомер, Месяц, КодДохода, КодВычета
having sum(СуммаДохода) > 0
order by ТабельныйНомер, Месяц, КодДохода

update TNDFL6_Header
set SummaVichet13 = isnull(SummaVichet13, 0) + isnull(s13, 0),
	SummaVichet30 = isnull(SummaVichet30, 0) + isnull(s30, 0)
from TNDFL6_Header,
	(
		select sum(case when isnull(#ZPNL.Нерезидент, 0) = 0 then isnull(#Tdoh.ДВ,0) else 0 end) as s13,
			sum(case when isnull(#ZPNL.Нерезидент, 0) <> 0 then isnull(#Tdoh.ДВ,0) else 0 end) as s30
		from #Tdoh
		inner join #Ttn on #Ttn.ТабельныйНомер = #Tdoh.ТабельныйНомер
		left join #ZPNL on #ZPNL.ТабельныйНомер = #Tdoh.ТабельныйНомер
	) t
where TNDFL6_Header.[Id] = @Id

update TNDFL6_Header
set SummaVichet13 = isnull(SummaVichet13, 0) + isnull(s13, 0),
	SummaVichet30 = isnull(SummaVichet30, 0) + isnull(s30, 0)
from TNDFL6_Header,
	(
		select sum(case when isnull(#ZPNL.Нерезидент, 0) = 0 then isnull(#ZPNMESD.СуммаВычета,0) else 0 end) as s13,
			sum(case when isnull(#ZPNL.Нерезидент, 0) <> 0 then isnull(#ZPNMESD.СуммаВычета,0) else 0 end) as s30
		from #ZPNMESD
		inner join #Ttn on #Ttn.ТабельныйНомер = #ZPNMESD.ТабельныйНомер
		left join #ZPNL on #ZPNL.ТабельныйНомер = #ZPNMESD.ТабельныйНомер
		where #ZPNMESD.КодВычета between 300 and 330
	) t
where TNDFL6_Header.[Id] = @Id
-- /////////// Вычеты //////////////////////////////////////////////////////////

update TNDFL6_Header
set
	KolichSotrudn = t.KolichSotrudn,
	SummaNalogaFull = t.SummaNalogaFull,
	SummaNeUderzh = t.SummaNeUderzh,
	SummaVozvrat = t.SummaVozvrat
from
	(
		select
			count(*) as KolichSotrudn,
			sum(Подоходный) as SummaNalogaFull,
			sum(case when isnull(Расчитано,0)>(Подоходный+isnull(patent1.PatentSumma,0)) then isnull(Расчитано,0)-isnull(Подоходный,0)-isnull(patent1.PatentSumma,0) else 0 end) as SummaNeUderzh,
			sum(case when isnull(Расчитано,0)<isnull(Подоходный,0)+isnull(patent1.PatentSumma,0) then isnull(Подоходный,0)+isnull(patent1.PatentSumma,0)-isnull(Расчитано,0) else null end) as SummaVozvrat
		from #ZPNL
		inner join VSELECT_LS on VSELECT_LS.ТабельныйНомер = #ZPNL.ТабельныйНомер
		left outer join #ZPNPATENT patent1 on patent1.TN = #ZPNL.ТабельныйНомер
		where VSELECT_LS.ИдУч = @IdUch and VSELECT_LS.Ргод = @CYear
	) t
where TNDFL6_Header.[Id] = @Id

update TNDFL6_Header
set
	SummaDohoda13 = t.SummaDohoda,
	--SummaVichet13 = t.SummaVichet,
	SummaIschislNalog13 = t.SummaIschislNalog,
	SummaPatent13 = t.SummaPatent
from (
		select
			sum(#ZPNL.Совокупный) as SummaDohoda,
			sum(0) as SummaVichet,
			sum(#ZPNL.Расчитано) as SummaIschislNalog,
			sum(patent1.PatentSumma) as SummaPatent
		from #ZPNL
		inner join #Ttn on #Ttn.ТабельныйНомер = #ZPNL.ТабельныйНомер
		left outer join #ZPNPATENT patent1 on patent1.TN = #ZPNL.ТабельныйНомер
		--left outer join #ZPNMESDgrp vichmesh on vichmesh.ТабельныйНомер = #ZPNL.ТабельныйНомер
		where isnull(#ZPNL.Нерезидент, 0) = 0
	) t
where TNDFL6_Header.[Id] = @Id

update TNDFL6_Header
set
	SummaDohoda30 = t.SummaDohoda,
	--SummaVichet30 = t.SummaVichet,
	SummaIschislNalog30 = t.SummaIschislNalog,
	SummaPatent30 = t.SummaPatent
from (
		select
			sum(Совокупный) as SummaDohoda,
			sum(0) as SummaVichet,
			sum(Расчитано) as SummaIschislNalog,
			sum(patent1.PatentSumma) as SummaPatent
		from #ZPNL
		inner join #ZPNPATENT patent1 on patent1.TN = #ZPNL.ТабельныйНомер
		--inner join #ZPNMESDgrp vichmesh on vichmesh.ТабельныйНомер = #ZPNL.ТабельныйНомер
		inner join #Ttn on #Ttn.ТабельныйНомер = #ZPNL.ТабельныйНомер
		where isnull(#ZPNL.Нерезидент, 0) <> 0
	) t
where TNDFL6_Header.[Id] = @Id

--//////////////////////////////////////////
drop table #ZPNL
drop table #ZPNPATENT
drop table #ZPNMES
drop table #ZPNMESDVich
drop table #ZPNMESD
drop table #Tdoh
drop table #Tv
drop table #Ttn
drop table #ZPNMESV
drop table #ZPNMESD_t
--drop table #ZPNMESDgrp
--//////////////////////////////////////////

declare @StartMonth smallint, @StartYear smallint

if (@IncludeLastPrevMonth = 0) begin
	select @StartYear = @CYear, @StartMonth = @LastMonth - 2
end
else if (@IncludeLastPrevMonth = 1 and @LastMonth = 3) begin
	select @StartYear = @CYear - 1, @StartMonth = 12
end
else if (@IncludeLastPrevMonth = 1 and @LastMonth <> 3) begin
	select @StartYear = @CYear, @StartMonth = @LastMonth - 3
end

declare @inc table (
	[Yr] smallint,
	[Month] smallint,
	Dohod numeric(18,2),
	Nalog numeric(18,2),
	KodDohoda smallint
)

-- доходы
insert into @inc([Yr], [Month], Dohod, KodDohoda)
select РГод as [Yr], Рмесяц as [Month], sum(XLIST.Начислено) as Dohod, 
	min(case when S_6_KOD.КодНДФЛ in (2012, 2300) then S_6_KOD.КодНДФЛ else 2000 end) as KodDohoda
from VXLIST XLIST
inner join S_6_KOD on S_6_KOD.Код = XLIST.Код 
where
	(
		(@StartYear = @CYear and XLIST.РГод = @CYear and XLIST.Рмесяц >= @StartMonth and XLIST.Рмесяц <= @CMonth) or
		(@CYear > @StartYear and XLIST.РГод = @CYear and XLIST.Рмесяц <= @CMonth) or
		(@CYear > @StartYear and XLIST.РГод = @StartYear and XLIST.Рмесяц = @StartMonth)
	) and S_6_KOD.КодНДФЛ is not null
group by РГод, Рмесяц, (case when S_6_KOD.КодНДФЛ in (2012, 2300) then S_6_KOD.КодНДФЛ else 2000 end)
order by РГод, Рмесяц, (case when S_6_KOD.КодНДФЛ in (2012, 2300) then S_6_KOD.КодНДФЛ else 2000 end)

-- а налоги из других строк, лел
update @inc
set Nalog = nal
from @inc base,
	(
		select Ргод, Рмесяц, sum(XLIST.Удержано) as nal
		from VXLIST XLIST
		where
			(
				(@StartYear = @CYear and XLIST.РГод = @CYear and XLIST.Рмесяц >= @StartMonth and XLIST.Рмесяц <= @CMonth) or
				(@CYear > @StartYear and XLIST.РГод = @CYear and XLIST.Рмесяц <= @CMonth) or
				(@CYear > @StartYear and XLIST.РГод = @StartYear and XLIST.Рмесяц = @StartMonth)
			) and (XLIST.Код in (82, 282, 382))
		group by Ргод, Рмесяц
	) t
where t.Ргод = base.Yr and t.Рмесяц = base.[Month] and KodDohoda = 2000

update @inc
set
	Nalog = case KodDohoda
		when 2000 then nFull * d1 / (d1 + d2 + d3)
		when 2012 then nFull * d2 / (d1 + d2 + d3)
		when 2300 then nFull * d3 / (d1 + d2 + d3)
	end
from @inc base
join (
	select
		Yr, [Month],
		sum(Nalog) as nFull,
		sum(case when KodDohoda = 2000 then Dohod else 0 end) as d1,
		sum(case when KodDohoda = 2012 then Dohod else 0 end) as d2,
		sum(case when KodDohoda = 2300 then Dohod else 0 end) as d3
	from @inc
	group by Yr, [Month]
	) t on t.[Month] = base.[Month] and t.Yr = base.Yr

delete from TNDFL6_P2 where [Id] = @Id

set dateformat dmy

declare @PaymentDay smallint
select @PaymentDay = isnull(ДеньЗарплаты, 0) from VA1NAST
if (@PaymentDay >= 30)
	select @PaymentDay = 29

delete from TNDFL6_P2 where [Id] = @Id
insert into TNDFL6_P2 (IdTNDFL6_Header, [Month], DohodType, DatePoluch, DateUderzh, DatePerechisl, SummaDohod, SummaNalog)
select
	@Id, [Month], KodDohoda,
	dateadd(day, -1, case when [Month]=12 then dbo.fmo([Yr]+1, 1) else dbo.fmo([Yr], [Month]+1) end) as DatePoluch,
	case
		when (@PaymentDay = 0 or KodDohoda <> 2000) then
			dateadd(day, -1, case when [Month]=12 then dbo.fmo([Yr]+1, 1) else dbo.fmo([Yr], [Month]+1) end)
		else
			dateadd(month, 1, dateadd(day, @PaymentDay-1, dbo.fmo([Yr], [Month])))
	end as DateUderzh,
	case
		when (@PaymentDay = 0 or KodDohoda <> 2000) then
			dateadd(day, -1, case when [Month]=12 then dbo.fmo([Yr]+1, 1) else dbo.fmo([Yr], [Month]+1) end)
		else
			dateadd(month, 1, dateadd(day, @PaymentDay-0, dbo.fmo([Yr], [Month])))
	end as DatePerechisl,
	Dohod, Nalog
from @inc
GO
