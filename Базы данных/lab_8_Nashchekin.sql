-- Чтоб можно было прогнать скрипт несколько раз
USE master;
GO

IF DB_ID (N'lab8') IS NOT NULL
DROP DATABASE lab8;
GO

-- Создание базы
CREATE DATABASE lab8
	ON (
		NAME = lab8_dat, 
		FILENAME = '/home/relict/desktop/db_labs/db_lab_8/labdat.mdf', 
		SIZE = 10MB, 
		MAXSIZE = UNLIMITED, 
		FILEGROWTH = 10%
	)
	LOG ON (
		NAME = lab8_log, 
		FILENAME = '/home/relict/desktop/db_labs/db_lab_8/lablog.ldf', 
		SIZE = 5MB, 
		MAXSIZE = 20MB, 
		FILEGROWTH = 5%
	);
GO

USE lab8;
GO

CREATE TABLE Galaxy(
	GalaxyID INT PRIMARY KEY IDENTITY(1, 1) NOT NULL,
	NGCNumber Varchar(8) NOT NULL,
	Type INT NOT NULL,
	Size INT NOT NULL,
	Constellation INT NOT NULL,
	ApparentMagnitude Numeric(4, 2) NOT NULL
);
GO

CREATE TABLE Star(
	StarID INT PRIMARY KEY IDENTITY(1, 1) NOT NULL,
	StarCatalogNumber Varchar(8) NOT NULL,
	Size Numeric(7, 2) NOT NULL,
	Mass Numeric(7, 2) NULL,
	Constellation INT NOT NULL,
	Temperature INT NULL,
	Luminosity Numeric(7, 2) NOT NULL,
	SpectralClass INT NOT NULL,
	ApparentMagnitude Numeric(4, 2) NOT NULL,
	GalaxyID INT NULL,
	FOREIGN KEY (GalaxyID) REFERENCES Galaxy(GalaxyID)
);
GO

INSERT INTO Galaxy(NGCNumber, Type, Size, Constellation, ApparentMagnitude)
	VALUES ('NGC1234', 6, 12, 3, 6);

INSERT INTO Galaxy(NGCNumber, Type, Size, Constellation, ApparentMagnitude)
	VALUES ('NGC1378', 3, 2, 38, 6.5);

INSERT INTO Galaxy(NGCNumber, Type, Size, Constellation, ApparentMagnitude)
	VALUES ('NGC138', 5, 8, 31, 16);

INSERT INTO Star(StarCatalogNumber, Size, Mass,
	Constellation, Temperature, Luminosity,
	SpectralClass, ApparentMagnitude, GalaxyID)
	VALUES('NGC111', 3.5, 5, 3, 17000, 1, 2, 3.5, 1);
	
INSERT INTO Star(StarCatalogNumber, Size, Mass,
	Constellation, Temperature, Luminosity,
	SpectralClass, ApparentMagnitude, GalaxyID)
	VALUES('NGC113', 6, 31, 38, 10000, 3, 6, 12, 2);
GO	


-- 1. Хранимая процедура (выборка и результат в виде курсора)
PRINT ''
GO

CREATE PROCEDURE GetCursorGalaxiesSizeAndMagnitude
	@Result cursor VARYING OUTPUT
AS
BEGIN
	SET @Result = CURSOR SCROLL FOR
	SELECT NGCNumber, Type, ApparentMagnitude
    FROM Galaxy;
   
	OPEN @Result;
	RETURN 
END;
GO

BEGIN 
	DECLARE @ResCursor cursor;
	EXECUTE GetCursorGalaxiesSizeAndMagnitude @Result = @ResCursor OUTPUT;
	
	DECLARE @NGCNumber Varchar(8), @Size int, @Magnitude Numeric(4, 2);

	FETCH NEXT FROM @ResCursor INTO @NGCNumber, @Size, @Magnitude;
	
	WHILE @@FETCH_STATUS = 0
	BEGIN
	    PRINT 'Номер в каталоге NGC: ' + @NGCNumber 
	        + ', Размер: ' + CAST(@Size as varchar) + ', Видимая звёздная величина: '
	        + CAST(@Magnitude as varchar);
	    FETCH NEXT FROM @ResCursor INTO @NGCNumber, @Size, @Magnitude;
	END;
	
	CLOSE @ResCursor;
	DEALLOCATE @ResCursor;
END;


-- 2. Выборка с использованием столбца, сформированного еще одной функцией
PRINT ''
GO



CREATE FUNCTION ReduceNGC (@NGCNumber Varchar(8))
RETURNS Varchar(5) 
AS
BEGIN
    DECLARE @WithoutNGC Varchar(5);
    SELECT @WithoutNGC = RIGHT(@NGCNumber, LEN(@NGCNumber) - 3);
RETURN @WithoutNGC
END;
GO



CREATE PROCEDURE GetCursorGalaxiesSizeAndMagnitudeMod
	@Result cursor VARYING OUTPUT
AS
BEGIN
	SET @Result = CURSOR FOR
	SELECT dbo.ReduceNGC(NGCNumber) as NGCNumberWithoutNGC, Type, ApparentMagnitude
    FROM Galaxy;
   
	OPEN @Result;
	RETURN 
END;
GO


BEGIN 
	DECLARE @ResCursor cursor;
	EXECUTE GetCursorGalaxiesSizeAndMagnitudeMod @Result = @ResCursor OUTPUT;
	
	DECLARE @NGCNumber Varchar(5), @Size int, @Magnitude Numeric(4, 2);
	
	FETCH NEXT FROM @ResCursor INTO @NGCNumber, @Size, @Magnitude;
	
	WHILE @@FETCH_STATUS = 0
	BEGIN
	    PRINT 'Номер в каталоге NGC (обновлённый): ' + 
	    	@NGCNumber + ', Размер: ' + CAST(@Size as varchar) + 
	    	', Видимая звёздная величина: ' + 
	    	CAST(@Magnitude as varchar);
	   
	    FETCH NEXT FROM @ResCursor INTO @NGCNumber, @Size, @Magnitude;
	END;
	
	CLOSE @ResCursor;
	DEALLOCATE @ResCursor;
END;

-- 3. Процедура для прокуртки возвращаемого курсора
PRINT ''
GO



CREATE FUNCTION IsVisibleForEye (@ApparentMagnitude Numeric(4, 2))
RETURNS bit 
AS
BEGIN
	IF @ApparentMagnitude > 7.0
		RETURN 0
	RETURN 1
END;
GO


CREATE PROCEDURE GetVisibleForEyeGalaxies
AS
BEGIN
	DECLARE @CursorFromFirstProc CURSOR;
	EXECUTE GetCursorGalaxiesSizeAndMagnitude @Result = @CursorFromFirstProc OUTPUT;
   	

	DECLARE @NGCNumber Varchar(8), @Size int, @Magnitude Numeric(4, 2);

	FETCH FIRST FROM @CursorFromFirstProc INTO @NGCNumber, @Size, @Magnitude;
	
	WHILE @@FETCH_STATUS = 0
	BEGIN
		
		
		IF dbo.IsVisibleForEye(@Magnitude) = 1
			PRINT 'Номер в каталоге NGC: ' + @NGCNumber + 
		    ', Размер: ' + CAST(@Size as varchar) + 
		    ', Видимая звёздная величина (можно увидеть невооружённым глазом): ' + 
		    CAST(@Magnitude as varchar);   
	   
	    FETCH NEXT FROM @CursorFromFirstProc INTO @NGCNumber, @Size, @Magnitude;
	END;

	
	CLOSE @CursorFromFirstProc;
	DEALLOCATE @CursorFromFirstProc;
END;
GO

EXECUTE GetVisibleForEyeGalaxies;
GO



-- 4. Модификация процедуры с использованием табличной фукнции
PRINT ''
GO


CREATE FUNCTION GalaxyTableFunction()
RETURNS @ResultTable TABLE (
    NGCNumberWithoutNGC Varchar(5),
    Type INT,
    ApparentMagnitude FLOAT
)
AS
BEGIN
    INSERT INTO @ResultTable (NGCNumberWithoutNGC, Type, ApparentMagnitude)
    SELECT dbo.ReduceNGC(NGCNumber), Type, ApparentMagnitude
    FROM Galaxy;
    RETURN;
END;
GO

CREATE FUNCTION GalaxyTableFunction2()
RETURNS table 
AS
RETURN (
    SELECT dbo.ReduceNGC(NGCNumber) as NGCNumberWithoutNGC, Type, ApparentMagnitude
    FROM Galaxy
);
GO


CREATE PROCEDURE GetCursorGalaxiesSizeAndMagnitudeMod2
	@Result cursor VARYING OUTPUT
AS
BEGIN
	SET @Result = CURSOR FOR
	SELECT * 
	FROM dbo.GalaxyTableFunction2();
   
	OPEN @Result;
	RETURN 
END;
GO


BEGIN
	DECLARE @ResCursor cursor;
	EXECUTE GetCursorGalaxiesSizeAndMagnitudeMod2 @Result = @ResCursor OUTPUT;
	
	DECLARE @NGCNumber Varchar(5), @Size int, @Magnitude Numeric(4, 2);
	
	FETCH NEXT FROM @ResCursor INTO @NGCNumber, @Size, @Magnitude;
	
	WHILE @@FETCH_STATUS = 0
	BEGIN
	    PRINT 'Номер в каталоге NGC (обновлённый): ' + 
	    	@NGCNumber + ', Размер: ' + CAST(@Size as varchar) + 
	    	', Видимая звёздная величина: ' + 
	    	CAST(@Magnitude as varchar);
	   
	    FETCH NEXT FROM @ResCursor INTO @NGCNumber, @Size, @Magnitude;
	END;
	
	CLOSE @ResCursor;
	DEALLOCATE @ResCursor;
END;



