-- Чтоб можно было прогнать скрипт несколько раз
USE master;
GO

IF DB_ID (N'lab7') IS NOT NULL
DROP DATABASE lab7;
GO

-- Создание базы
CREATE DATABASE lab7
	ON (
		NAME = lab7_dat, 
		FILENAME = '/home/relict/desktop/db_labs/db_lab_7/labdat.mdf', 
		SIZE = 10MB, 
		MAXSIZE = UNLIMITED, 
		FILEGROWTH = 10%
	)
	LOG ON (
		NAME = lab7_log, 
		FILENAME = '/home/relict/desktop/db_labs/db_lab_7/lablog.ldf', 
		SIZE = 5MB, 
		MAXSIZE = 20MB, 
		FILEGROWTH = 5%
	);
GO

USE lab7;
GO

CREATE TABLE Galaxy(
	GalaxyID INT PRIMARY KEY IDENTITY(1, 1) NOT NULL,
	NGCNumber Varchar(8) NOT NULL,
	Type INT NOT NULL,
	Size INT NOT NULL,
	Constellation INT NOT NULL,
	ApparentMagnitude Numeric(4, 2) NOT NULL,
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
	FOREIGN KEY (GalaxyID) REFERENCES Galaxy(GalaxyID),
);
GO

INSERT INTO Galaxy(NGCNumber, Type, Size, Constellation, ApparentMagnitude)
	VALUES ('NGC1234', 6, 12, 3, 8);

INSERT INTO Galaxy(NGCNumber, Type, Size, Constellation, ApparentMagnitude)
	VALUES ('NGC1378', 3, 2, 38, 10);

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


-- 1. Представление для одной таблицы

CREATE VIEW Star_view AS
SELECT
	s.StarCatalogNumber, s.Size, s.Mass,
	s.Constellation, s.Temperature, s.Luminosity,
	s.SpectralClass, s.ApparentMagnitude, s.GalaxyID
FROM Star AS s
GO

SELECT * FROM Star_view
GO


-- 2. Представление для двух таблиц

CREATE VIEW Star_Galaxy_view AS
SELECT
	s.StarCatalogNumber, s.Size as StarSize, s.Mass,
	s.Temperature, s.ApparentMagnitude, s.GalaxyID,
	g.NGCNumber, g.Type, g.Size as GalaxySize, g.Constellation
FROM Star AS s
INNER JOIN Galaxy AS g
ON s.GalaxyID = g.GalaxyID
GO

SELECT * FROM Star_Galaxy_view
GO


-- 3. Индекс для таблицы

CREATE INDEX IX_Star
ON Star(Size, Mass)
INCLUDE (StarCatalogNumber, Temperature)
GO

SELECT Size, Mass, StarCatalogNumber, Temperature
FROM Star
WHERE Size > 4
GO


-- 4. Индексированное представление

CREATE VIEW Galaxy_Indexed_View WITH SCHEMABINDING AS
	SELECT NGCNumber, Size, Type,
		Constellation, ApparentMagnitude
	FROM dbo.Galaxy
	WHERE ApparentMagnitude < 14
GO
CREATE UNIQUE CLUSTERED INDEX IX_galaxy ON Galaxy_Indexed_View(NGCNumber)
GO

SELECT * FROM Galaxy_Indexed_View
GO






