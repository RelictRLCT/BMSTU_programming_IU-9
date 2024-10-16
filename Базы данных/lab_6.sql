-- Чтоб можно было прогнать скрипт несколько раз
USE master;
GO

IF DB_ID (N'lab6') IS NOT NULL
DROP DATABASE lab6;
GO

-- Создание базы
CREATE DATABASE lab6
	ON (
		NAME = lab6_dat, 
		FILENAME = '/home/relict/desktop/db_lab_6/labdat.mdf', 
		SIZE = 10MB, 
		MAXSIZE = UNLIMITED, 
		FILEGROWTH = 10%
	)
	LOG ON (
		NAME = lab6_log, 
		FILENAME = '/home/relict/desktop/db_lab_6/lablog.ldf', 
		SIZE = 5MB, 
		MAXSIZE = 20MB, 
		FILEGROWTH = 5%
	);
GO

USE lab6;
GO

-- 1. Создание таблицы с IDENTITY
CREATE TABLE Galaxy(
	GalaxyID INT PRIMARY KEY IDENTITY(1, 1) NOT NULL,
	NGCNumber Varchar(8) NOT NULL
		CONSTRAINT DF_NGCNumber DEFAULT ('NGC?'),
	Type INT NOT NULL,
	Size INT NOT NULL,
	Constellation INT NOT NULL,
	ApparentMagnitude Numeric(4, 2) NOT NULL,
	
	-- такого поля в 3 лабе нет, оно только для использования встроенной функции вычисления
	DateOfInsert Datetime NOT NULL
		CONSTRAINT DF_Date DEFAULT (GETDATE()),
	
	-- в самой полной классификации 18 типов
	CONSTRAINT CHK_Type
		CHECK ([Type] >= 1 AND [Type] <= 18),
		
	-- 88 созвездий
	CONSTRAINT CHK_Constellation_Galaxy
		CHECK ([Constellation] >= 1 AND [Constellation] <= 88)
	
);
GO

INSERT INTO Galaxy(NGCNumber, Type, Size, Constellation, ApparentMagnitude) VALUES ('NGC123', 1, 2, 3, 4.6);
-- SCOPE_IDENTITY() возвращает последний идентификатор, созданный в том же сеансе и в той же области видимости.
SELECT SCOPE_IDENTITY() AS LastIdentityFromScope

INSERT INTO Galaxy(NGCNumber, Type, Size, Constellation, ApparentMagnitude) VALUES ('NGC127', 1, 87, 2, 8);
-- @@IDENTITY возвращает последний идентификатор, созданный в том же сеансе.
SELECT @@IDENTITY  AS LastIdentityFromIDENTITY

INSERT INTO Galaxy(NGCNumber, Type, Size, Constellation, ApparentMagnitude) VALUES ('NGC3476', 3, 100, 1, 9.1);
-- IDENT_CURRENT('TABLE') возвращает последний идентификатор, 
-- созданный для определенной таблицы в любом сеансе. 
SELECT IDENT_CURRENT('GALAXY') AS LastIdentityFromIDENT_CURRENT
GO


-- 2. CHECK, DEFAULT, вычисляемые функции
-- CHECK не даст вставить неверные значения Type
-- INSERT INTO Galaxy(NGCNumber, Type, Size, Constellation, ApparentMagnitude) VALUES ('NGC123', -1, 2, 3, 4.6);
-- INSERT INTO Galaxy(NGCNumber, Type, Size, Constellation, ApparentMagnitude) VALUES ('NGC123', 14, 2, 3, 4.6);
INSERT INTO Galaxy(Type, Size, Constellation, ApparentMagnitude) VALUES (5, 12, 5, 8);


-- 3. Создание таблицы с первичным ключом в виде глобального идентификатора
CREATE TABLE Star(
	StarID UNIQUEIDENTIFIER DEFAULT NEWID() PRIMARY KEY NOT NULL,
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
	
	-- 88 созвездий
	CONSTRAINT CHK_Constellation_Star
		CHECK ([Constellation] >= 1 AND [Constellation] <= 88),
		
	-- 7 спектральных классов
	CONSTRAINT CHK_SpecrtalClass
		CHECK ([SpectralClass] >= 1 AND [SpectralClass] <= 7)
);
GO

INSERT INTO Star(StarCatalogNumber, Size, Mass, Constellation, 
			Temperature, Luminosity, SpectralClass, ApparentMagnitude)
VALUES ('NGC111', 3.5, 5, 3, 17000, 1, 2, 3.5);

SELECT * FROM Star;
GO


-- 4. Создание таблицы с первичным ключом на основе последовательности
CREATE SEQUENCE CountBy1
START WITH 1
INCREMENT BY 1;
GO

CREATE TABLE Planet(
	PlanetID INT PRIMARY KEY NOT NULL,
	PlanetCatalogNumber Varchar(8) NOT NULL,
	Size Numeric(7, 2) NOT NULL,
	Mass Numeric(7, 2) NULL,
	Temperature INT NULL,
	CompositionType INT NULL,
	ApparentMagnitude Numeric(4, 2) NOT NULL,
);
GO

INSERT INTO Planet(PlanetID, PlanetCatalogNumber, Size, 
				Mass, Temperature, CompositionType, ApparentMagnitude)
VALUES(NEXT VALUE FOR CountBy1, 'NGC1456', 2.4, 4.6, 500, 2, 2.3);

INSERT INTO Planet(PlanetID, PlanetCatalogNumber, Size, 
				Mass, Temperature, CompositionType, ApparentMagnitude)
VALUES(NEXT VALUE FOR CountBy1, 'NGC1356', 1, 1, 300, 1, 1);

SELECT * FROM Planet;
GO

-- 5. Ограничения ссылочной целостности 
DROP TABLE Planet;
DROP TABLE Star;
DROP TABLE Galaxy;

CREATE TABLE Galaxy(
	GalaxyID INT PRIMARY KEY NOT NULL
		CONSTRAINT DF_GalaxyID DEFAULT (1),
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
	GalaxyID INT NULL
		CONSTRAINT DF_GalaxyID_Star DEFAULT (1),
	FOREIGN KEY (GalaxyID) REFERENCES Galaxy(GalaxyID)
	ON DELETE SET NULL 
	-- ON DELETE SET DEFAULT
	-- ON DELETE NO ACTION
	-- ON DELETE CASCADE
);
GO

INSERT INTO Galaxy(NGCNumber, Type, Size, Constellation, ApparentMagnitude) 
VALUES ('NGC3476', 3, 100, 1, 9.1);
INSERT INTO Galaxy(GalaxyID, NGCNumber, Type, Size, Constellation, ApparentMagnitude) 
VALUES (2, 'NGC3576', 4, 79, 2, 8);

INSERT INTO Star(StarCatalogNumber, Size, Mass, Constellation, 
			Temperature, Luminosity, SpectralClass, ApparentMagnitude, GalaxyID)
VALUES ('NGC111', 3.5, 5, 3, 17000, 1, 2, 3.5, 1);
INSERT INTO Star(StarCatalogNumber, Size, Mass, Constellation, 
			Temperature, Luminosity, SpectralClass, ApparentMagnitude, GalaxyID)
VALUES ('NGC112', 3, 6, 5, 19000, 3, 2, 3, 2);

SELECT StarID, GalaxyID FROM Star;

DELETE Galaxy WHERE GalaxyID = 2;

SELECT StarID, GalaxyID FROM Star;

