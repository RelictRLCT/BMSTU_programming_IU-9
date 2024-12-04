-- Чтоб можно было прогнать скрипт несколько раз
USE master;
GO

IF DB_ID (N'lab11') IS NOT NULL
DROP DATABASE lab11;
GO

-- Создание базы
CREATE DATABASE lab11
	ON (
		NAME = lab11_dat, 
		FILENAME = '/home/relict/desktop/db_labs/db_lab_11/labdat.mdf',
		SIZE = 10MB, 
		MAXSIZE = UNLIMITED, 
		FILEGROWTH = 10%
	)
	LOG ON (
		NAME = lab11_log, 
		FILENAME = '/home/relict/desktop/db_labs/db_lab_11/lablog.ldf',
		SIZE = 5MB, 
		MAXSIZE = 20MB, 
		FILEGROWTH = 5%
	);
GO

USE lab11;
GO

-- Проверка формата номера каталога
CREATE FUNCTION NGCCheck (@NGCNumber VARCHAR(8))
RETURNS BIT AS
    BEGIN
        IF (LEN(@NGCNumber) > 3 AND @NGCNumber LIKE 'NGC%')
            RETURN 1
        RETURN 0
    END;
GO


CREATE TABLE Galaxy(
	GalaxyID INT PRIMARY KEY IDENTITY(1, 1) NOT NULL,
	NGCNumber Varchar(8) UNIQUE NOT NULL,
	Type INT NOT NULL,
	Size INT NOT NULL,
	Constellation INT NOT NULL,
	ApparentMagnitude Numeric(4, 2) NOT NULL,

    -- начинается на NGC
    CONSTRAINT CHK_NGCNum_Format
        CHECK (dbo.NGCCheck(NGCNumber)=1),

	-- в самой полной классификации 18 типов
	CONSTRAINT CHK_Type
		CHECK ([Type] >= 1 AND [Type] <= 18),
		
	-- 88 созвездий
	CONSTRAINT CHK_Constellation_Galaxy
		CHECK ([Constellation] >= 1 AND [Constellation] <= 88)
	
);
GO

CREATE TABLE Star(
	StarID INT PRIMARY KEY IDENTITY(1, 1) NOT NULL,
	StarCatalogNumber Varchar(8) UNIQUE NOT NULL,
	Size Numeric(7, 2) NOT NULL,
	Mass Numeric(7, 2) NULL,
	Constellation INT NOT NULL,
	Temperature INT NULL,
	Luminosity Numeric(7, 2) NOT NULL,
	SpectralClass INT NOT NULL,
	ApparentMagnitude Numeric(4, 2) NOT NULL,
	GalaxyID INT NULL,
	FOREIGN KEY (GalaxyID) REFERENCES Galaxy(GalaxyID)
        ON DELETE CASCADE,

    -- начинается на NGC
    CONSTRAINT CHK_SCN_Format
        CHECK (dbo.NGCCheck(StarCatalogNumber)=1),

    -- 88 созвездий
	CONSTRAINT CHK_Constellation_Star
		CHECK ([Constellation] >= 1 AND [Constellation] <= 88),
		
	-- 7 спектральных классов
	CONSTRAINT CHK_SpecrtalClass
		CHECK ([SpectralClass] >= 1 AND [SpectralClass] <= 7)
);
GO


CREATE INDEX IX_Star
    ON Star(Size, Mass)
    INCLUDE (StarCatalogNumber, Temperature)
GO


CREATE TABLE Planet(
	PlanetID INT PRIMARY KEY IDENTITY(1, 1) NOT NULL,
	PlanetCatalogNumber Varchar(8) NOT NULL,
	Size Numeric(7, 2) NOT NULL,
	Mass Numeric(7, 2) NULL,
	Temperature INT NULL,
	CompositionType INT NULL,
	ApparentMagnitude Numeric(4, 2) NOT NULL,
);
GO

CREATE TABLE Star_Planet_Int(
	StarID INT NOT NULL,
	PlanetID INT NOT NULL,
    PRIMARY KEY (StarID, PlanetID),
	FOREIGN KEY (StarID) REFERENCES Star(StarID)
        ON DELETE CASCADE,
	FOREIGN KEY (PlanetID) REFERENCES Planet(PlanetID)
        ON DELETE CASCADE,
);
GO

CREATE TABLE Satellite(
	SatelliteCatalogNumber Varchar(10) PRIMARY KEY NOT NULL,
	Size Numeric(7, 2) NOT NULL,
	Mass Numeric(7, 2) NULL,
	PeriodCol Numeric(7, 2) NULL,
	ApparentMagnitude Numeric(4, 2) NOT NULL,
    PlanetID INT NOT NULL,
    FOREIGN KEY (PlanetID) REFERENCES Planet(PlanetID)
        ON DELETE CASCADE,
);
GO

CREATE TABLE NaturalSat(
	SatelliteCatalogNumber Varchar(10) PRIMARY KEY NOT NULL,
	Temperature Numeric(7, 2) NULL,
	CompositionType INT NULL,
	FOREIGN KEY (SatelliteCatalogNumber) REFERENCES Satellite(SatelliteCatalogNumber)
	    ON DELETE CASCADE,
);
GO

CREATE TABLE ArtificialSat(
	SatelliteCatalogNumber Varchar(10) PRIMARY KEY NOT NULL,
	Type INT NOT NULL,
	HeightAboveSurface Numeric(7, 2) NOT NULL,
	FOREIGN KEY (SatelliteCatalogNumber) REFERENCES Satellite(SatelliteCatalogNumber)
	    ON DELETE CASCADE,
);
GO

CREATE TRIGGER DeleteCancelArtSatTrigger ON ArtificialSat
    AFTER DELETE
    AS
BEGIN
    IF TRIGGER_NESTLEVEL() > 1
        BEGIN
            RETURN
        END

    RAISERROR(N'Удалять подтип отдельно запрещено ', 16, 5);
    ROLLBACK TRANSACTION;
END
GO

-- DELETE FROM ArtificialSat
-- WHERE Type = 2
-- GO

-- DELETE FROM SatelliteArtSubtypeView
-- WHERE SatelliteCatalogNumber = 'OSP9437'

INSERT INTO Galaxy(NGCNumber, Type, Size, Constellation, ApparentMagnitude)
VALUES
    ('NGC123', 1, 2, 3, 4.6),
    ('NGC127', 1, 87, 2, 8),
    ('NGC3476', 3, 100, 6, 9.1),
    ('NGC3676', 7, 68, 6, 12.6),
    ('NGC3976', 2, 187, 6, 13.7);
GO

-- INSERT INTO Galaxy(NGCNumber, Type, Size, Constellation, ApparentMagnitude)
-- VALUES
--     ('NG123', 1, 2, 3, 4.6),
--     ('NGC127', 1, 87, 98, 8),
--     ('NGC3476', 32, 100, 6, 9.1)
-- GO

SELECT * FROM Galaxy;

INSERT INTO Star(StarCatalogNumber, Size, Mass, Constellation,
                 Temperature, Luminosity, SpectralClass, ApparentMagnitude)
VALUES
    ('NGC111', 3.5, 2, 3, 17000, 1, 2, 3.5),
    ('NGC112', 4, 6, 7, 10000, 2, 2, 3.5),
    ('NGC113', 18, 100, 5, 19000, 4, 2, 3.5);

INSERT INTO Star(StarCatalogNumber, Size, Mass, Constellation,
                 Temperature, Luminosity, SpectralClass, ApparentMagnitude, GalaxyID)
VALUES
    ('NGC114', 10, 25, 14, 11000, 2, 1, 6, 1);

INSERT INTO Star(StarCatalogNumber, Size, Mass, Constellation,
                 Temperature, Luminosity, SpectralClass, ApparentMagnitude, GalaxyID)
VALUES
    ('NGC115', 8, 20, 12, 9000, 3, 5, 7,
     (SELECT GalaxyID FROM Galaxy WHERE NGCNumber = 'NGC127'));

DELETE FROM Galaxy
WHERE NGCNumber='NGC127'

INSERT INTO Planet(PlanetCatalogNumber, Size,
                   Mass, Temperature, CompositionType, ApparentMagnitude)
VALUES
    ('TOI-5319', 2.4, 4.6, 500, 2, 23),
    ('TOI-2768', 1, 1, 300, 1, 15),
    ('JUP', 2.4, 4.6, 500, 2, 6),
    ('SAT', 1, 1, 300, 1, 7);;
GO

CREATE VIEW SatelliteArtSubtypeView AS
SELECT
    s.SatelliteCatalogNumber, s.Size, s.Mass,
    s.PeriodCol, s.ApparentMagnitude, s.PlanetID, a.Type,
    a.HeightAboveSurface
FROM Satellite AS s
         INNER JOIN ArtificialSat AS a
                   ON s.SatelliteCatalogNumber = a.SatelliteCatalogNumber
GO

-- INSERT триггер для Artificial subtype
CREATE TRIGGER SatelliteArtSubtypeViewInsertTrigger ON SatelliteArtSubtypeView
    INSTEAD OF INSERT
    AS
BEGIN

    INSERT INTO Satellite(SatelliteCatalogNumber, Size, Mass, PeriodCol, ApparentMagnitude, PlanetID)
        (SELECT SatelliteCatalogNumber, Size, Mass, PeriodCol, ApparentMagnitude, PlanetID FROM inserted)

    INSERT INTO ArtificialSat(SatelliteCatalogNumber, Type, HeightAboveSurface)
        (SELECT SatelliteCatalogNumber, Type, HeightAboveSurface FROM inserted)

END;
GO


INSERT INTO SatelliteArtSubtypeView(SatelliteCatalogNumber, Size, Mass, PeriodCol,
                                    ApparentMagnitude, Type, HeightAboveSurface, PlanetID)
VALUES ('OSP9437', 0.2, 0.1, 3, 12, 2, 2000, 1);
GO

INSERT INTO SatelliteArtSubtypeView(SatelliteCatalogNumber, Size, Mass, PeriodCol,
                                    ApparentMagnitude, Type, HeightAboveSurface, PlanetID)
VALUES ('OSP9440', 0.5, 0.3, 4, 11, 4, 2200, 2),
       ('OSP9448', 0.9, 2, 6, 17, 2, 2000, 2),
       ('OSP9449', 0.3, 0.1, 12, 16, 2, 2600, 2);
GO

SELECT * FROM SatelliteArtSubtypeView
GO


-- UPDATE триггер для Artificial subtype
CREATE TRIGGER SatelliteArtSubViewUpdateTrigger ON SatelliteArtSubtypeView
    INSTEAD OF UPDATE
    AS
BEGIN
    IF UPDATE(SatelliteCatalogNumber)
        RAISERROR(N'Запрещено менять номер в каталоге ', 16, 4)
    RETURN;

    IF UPDATE(PlanetID)
        RAISERROR(N'Запрещено менять планету ', 16, 5)
    RETURN;

    UPDATE Satellite
    SET
        Size = inserted.Size,
        Mass = inserted.Mass,
        PeriodCol = inserted.PeriodCol,
        ApparentMagnitude = inserted.ApparentMagnitude
    FROM Satellite
             INNER JOIN inserted ON Satellite.SatelliteCatalogNumber = inserted.SatelliteCatalogNumber;

    UPDATE ArtificialSat
    SET
        Type = inserted.Type,
        HeightAboveSurface = inserted.HeightAboveSurface
    FROM ArtificialSat
             INNER JOIN inserted ON ArtificialSat.SatelliteCatalogNumber = inserted.SatelliteCatalogNumber;
END;
GO


UPDATE SatelliteArtSubtypeView
SET Size = 123, Type = 3
WHERE SatelliteCatalogNumber = 'OSP9437'
GO

SELECT * FROM SatelliteArtSubtypeView
GO

-- DELETE триггер для Artificial subtype
CREATE TRIGGER SatelliteArtSubViewDeleteTrigger ON SatelliteArtSubtypeView
    INSTEAD OF DELETE
    AS
BEGIN
    -- сработает каскадное удаление Artificial
    DELETE FROM Satellite
    WHERE SatelliteCatalogNumber IN (
        SELECT SatelliteCatalogNumber FROM deleted
    )

END
GO

DELETE FROM SatelliteArtSubtypeView
WHERE SatelliteCatalogNumber = 'OSP9440'
GO

SELECT * FROM SatelliteArtSubtypeView
GO


CREATE VIEW SatelliteNatSubtypeView AS
SELECT
    s.SatelliteCatalogNumber, s.Size, s.Mass,
    s.PeriodCol, s.ApparentMagnitude, s.PlanetID, n.CompositionType,
    n.Temperature
FROM Satellite AS s
         INNER JOIN NaturalSat AS n
                   ON s.SatelliteCatalogNumber = n.SatelliteCatalogNumber
GO

-- INSERT триггер для Natural subtype
CREATE TRIGGER SatelliteNatSubtypeViewInsertTrigger ON SatelliteNatSubtypeView
    INSTEAD OF INSERT
    AS
BEGIN

    INSERT INTO Satellite(SatelliteCatalogNumber, Size, Mass, PeriodCol, ApparentMagnitude, PlanetID)
        (SELECT SatelliteCatalogNumber, Size, Mass, PeriodCol, ApparentMagnitude, PlanetID FROM inserted)

    INSERT INTO NaturalSat(SatelliteCatalogNumber, CompositionType, Temperature)
        (SELECT SatelliteCatalogNumber, CompositionType, Temperature FROM inserted)

END;
GO

INSERT INTO SatelliteNatSubtypeView(SatelliteCatalogNumber, Size, Mass, PeriodCol,
                                    ApparentMagnitude, CompositionType, Temperature, PlanetID)
VALUES
    ('SAT-II', 0.2, 0.1, 3, 12, 2, 200, 3),
    ('JUP-X', 0.5, 0.3, 4, 11, 4, 2200, 2);
GO

-- UPDATE триггер для Natural subtype
CREATE TRIGGER SatelliteNatSubViewUpdateTrigger ON SatelliteNatSubtypeView
    INSTEAD OF UPDATE
    AS
BEGIN
    IF UPDATE(SatelliteCatalogNumber)
        RAISERROR(N'Запрещено менять номер в каталоге ', 16, 4)
    RETURN;

    UPDATE Satellite
    SET
        Size = inserted.Size,
        Mass = inserted.Mass,
        PeriodCol = inserted.PeriodCol,
        ApparentMagnitude = inserted.ApparentMagnitude
    FROM Satellite
             INNER JOIN inserted ON Satellite.SatelliteCatalogNumber = inserted.SatelliteCatalogNumber;

    UPDATE NaturalSat
    SET
        CompositionType = inserted.CompositionType,
        Temperature = inserted.Temperature
    FROM NaturalSat
             INNER JOIN inserted ON NaturalSat.SatelliteCatalogNumber = inserted.SatelliteCatalogNumber;
END;
GO

-- DELETE триггер для Natural subtype
CREATE TRIGGER SatelliteNatSubViewDeleteTrigger ON SatelliteNatSubtypeView
    INSTEAD OF DELETE
    AS
BEGIN
    -- сработает каскадное удаление Natural
    DELETE FROM Satellite
    WHERE SatelliteCatalogNumber IN (
        SELECT SatelliteCatalogNumber FROM deleted
    )

END;
GO


-- JOIN (Inner в триггерах)
SELECT * FROM Galaxy
SELECT * FROM Star

SELECT g.NGCNumber AS GalaxyNumber, g.Constellation, s.StarCatalogNumber AS StarNumber
FROM Galaxy AS g
        LEFT JOIN Star s
                ON g.GalaxyID = s.GalaxyID


SELECT * FROM SatelliteArtSubtypeView
SELECT * FROM Planet

SELECT p.PlanetCatalogNumber, s.SatelliteCatalogNUmber
FROM Planet AS p
        RIGHT JOIN SatelliteArtSubtypeView AS s
                ON p.PlanetID=s.PlanetID



SELECT p.PlanetCatalogNumber, s.SatelliteCatalogNUmber
FROM Planet AS p
         FULL OUTER JOIN SatelliteArtSubtypeView AS s
                    ON p.PlanetID=s.PlanetID


-- Условия выбора

SELECT * FROM Galaxy AS g
WHERE g.NGCNumber LIKE 'NGC1%'


SELECT * FROM Star AS s
WHERE s.GalaxyID IS NOT NULL


SELECT * FROM Galaxy AS g
WHERE g.Size BETWEEN 1 AND 10


SELECT * FROM Star AS s
WHERE s.SpectralClass IN (1, 2, 3)


INSERT INTO Star_Planet_Int(StarID, PlanetID)
VALUES (1, 1)


SELECT * FROM Star AS s
WHERE EXISTS(
    SELECT * FROM Star_Planet_Int AS spi
             WHERE spi.StarID = s.StarID
)


-- Сортировка
SELECT * FROM Star AS s
        ORDER BY s.Size DESC


SELECT * FROM Galaxy AS g
         ORDER BY g.Size


-- Группировка
SELECT g.Constellation, COUNT(*) AS CountGalaxy FROM Galaxy AS g
GROUP BY g.Constellation
HAVING COUNT(*) < 10


SELECT g.Constellation, COUNT(DISTINCT g.Type) AS UniqueGalaxyTypes FROM Galaxy as g
GROUP BY g.Constellation


SELECT g.Constellation, AVG(g.Size) AS AverageGalaxySize FROM Galaxy AS g
GROUP BY g.Constellation


SELECT s.Constellation, SUM(s.Mass) AS StarMassSum FROM Star AS s
GROUP BY s.Constellation


SELECT g.Constellation, MIN(g.ApparentMagnitude) AS GalaxyMinApparentMag FROM Galaxy AS g
GROUP BY g.Constellation


SELECT g.Constellation, MAX(g.Size) AS GalaxyMaxSize FROM Galaxy AS g
GROUP BY g.Constellation



-- UNION / EXCEPT / INTERSECT

SELECT StarCatalogNumber AS CatalogNumber
FROM Star

UNION

SELECT NGCNumber
FROM Galaxy



SELECT StarCatalogNumber AS CatalogNumber
FROM Star

UNION ALL

SELECT NGCNumber
FROM Galaxy



SELECT StarCatalogNumber
FROM Star

EXCEPT

SELECT s.StarCatalogNumber
FROM Star AS s
         INNER JOIN Star_Planet_Int AS spi
                    ON s.StarID = spi.StarID



SELECT StarCatalogNumber
FROM Star

INTERSECT

SELECT s.StarCatalogNumber
FROM Star AS s
         INNER JOIN Star_Planet_Int AS spi
                    ON s.StarID = spi.StarID
GO

-- Вложенный запрос
SELECT * FROM Star WHERE Star.ApparentMagnitude <
                         (SELECT MIN(g.ApparentMagnitude) FROM Galaxy AS g)
