-- Чтоб можно было прогнать скрипт несколько раз
USE master;
GO

IF DB_ID (N'lab15_1') IS NOT NULL
    DROP DATABASE lab15_1;
GO

IF DB_ID (N'lab15_2') IS NOT NULL
    DROP DATABASE lab15_2;
GO

-- Создание баз
CREATE DATABASE lab15_1
    ON (
    NAME = lab15_1_dat,
    FILENAME = '/home/relict/desktop/db_labs/db_lab_15/labdat_1.mdf',
    SIZE = 10MB,
    MAXSIZE = UNLIMITED,
    FILEGROWTH = 10%
    )
    LOG ON (
    NAME = lab15_1_log,
    FILENAME = '/home/relict/desktop/db_labs/db_lab_15/lablog_1.ldf',
    SIZE = 5MB,
    MAXSIZE = 20MB,
    FILEGROWTH = 5%
    );
GO

CREATE DATABASE lab15_2
    ON (
    NAME = lab15_2_dat,
    FILENAME = '/home/relict/desktop/db_labs/db_lab_15/labdat_2.mdf',
    SIZE = 10MB,
    MAXSIZE = UNLIMITED,
    FILEGROWTH = 10%
    )
    LOG ON (
    NAME = lab15_2_log,
    FILENAME = '/home/relict/desktop/db_labs/db_lab_15/lablog_2.ldf',
    SIZE = 5MB,
    MAXSIZE = 20MB,
    FILEGROWTH = 5%
    );
GO

USE lab15_1;
GO

CREATE TABLE Galaxy(
       GalaxyID INT PRIMARY KEY IDENTITY(1, 1) NOT NULL,
       NGCNumber Varchar(8) UNIQUE NOT NULL,
       Type INT NOT NULL,
       Size INT NOT NULL,
       Constellation INT NOT NULL,
       ApparentMagnitude Numeric(4, 2) NOT NULL,

    -- в самой полной классификации 18 типов
       CONSTRAINT CHK_Type
           CHECK ([Type] >= 1 AND [Type] <= 18),

    -- 88 созвездий
       CONSTRAINT CHK_Constellation_Galaxy
           CHECK ([Constellation] >= 1 AND [Constellation] <= 88)

);
GO


USE lab15_2;
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

-- 88 созвездий
     CONSTRAINT CHK_Constellation_Star
         CHECK ([Constellation] >= 1 AND [Constellation] <= 88),

-- 7 спектральных классов
     CONSTRAINT CHK_SpecrtalClass
         CHECK ([SpectralClass] >= 1 AND [SpectralClass] <= 7)
);
GO


USE lab15_1;
GO

CREATE VIEW GalaxyStarView AS
SELECT
    g.NGCNumber AS GalaxyCatalogNumber, g.Type, g.Size AS GalaxySize,
    g.Constellation, g.ApparentMagnitude AS GalaxyAppMag, s.StarCatalogNumber,
    s.Size AS StarSize, s.Mass, s.Temperature, s.Luminosity,
    s.SpectralClass, s.ApparentMagnitude AS StarAppMag
FROM lab15_1.dbo.Galaxy AS g
         INNER JOIN lab15_2.dbo.Star AS s
                    ON g.GalaxyID = s.GalaxyID
GO

SELECT * FROM GalaxyStarView
GO
-- DELETE Galaxy
CREATE TRIGGER GalaxyDeleteTrigger ON lab15_1.dbo.Galaxy
    AFTER delete
    AS
BEGIN
    DELETE FROM lab15_2.dbo.Star
           WHERE GalaxyID IN (SELECT GalaxyID FROM deleted)
END
GO

USE lab15_2;
GO

-- INSERT Star
DROP TRIGGER StarInsertTrigger
CREATE TRIGGER StarInsertTrigger ON lab15_2.dbo.Star
    AFTER INSERT
    AS
BEGIN
    IF EXISTS (SELECT * FROM inserted AS i
                    WHERE i.GalaxyID NOT IN
                          (SELECT g.GalaxyID FROM lab15_1.dbo.Galaxy AS g))
    BEGIN
        RAISERROR(N'GalaxyID отсутсвтует в таблице Galaxy', 16, 1);
        ROLLBACK TRANSACTION;
    END;
END
GO


INSERT INTO lab15_1.dbo.Galaxy(NGCNumber, Type, Size, Constellation, ApparentMagnitude)
VALUES ('NGC11', 1, 2, 12, 11),
       ('NGC12', 2, 3, 13, 12),
       ('NGC13', 3, 4, 14, 13)

INSERT INTO lab15_2.dbo.Star(StarCatalogNumber, Size, Mass, Constellation, Temperature, Luminosity, SpectralClass, ApparentMagnitude, GalaxyID)
VALUES ('NGC111', 12, 120, 12, 11000, 1, 2, 18, 1),
       ('NGC112', 2, 12, 12, 18000, 4, 3, 14, 1),
       ('NGC113', 1, 10, 14, 12000, 1, 1, 10, NULL),
       ('NGC114', 2, 11, 11, 13000, 2, 1, 12, 2),
       ('NGC116', 2, 11, 11, 13000, 2, 1, 12, NULL)
GO

INSERT INTO lab15_2.dbo.Star(StarCatalogNumber, Size, Mass, Constellation, Temperature, Luminosity, SpectralClass, ApparentMagnitude, GalaxyID)
VALUES ('NGC116', 2, 11, 11, 13000, 2, 1, 12, NULL);

INSERT INTO lab15_2.dbo.Star(StarCatalogNumber, Size, Mass, Constellation, Temperature, Luminosity, SpectralClass, ApparentMagnitude, GalaxyID)
VALUES ('NGC127', 12, 120, 12, 11000, 1, 2, 18, 5)
GO

SELECT * FROM lab15_1.dbo.Galaxy
SELECT * FROM lab15_2.dbo.Star

-- UPDATE Star
CREATE TRIGGER StarUpdateTrigger ON lab15_2.dbo.Star
    AFTER UPDATE
    AS
BEGIN
    IF EXISTS (SELECT * FROM inserted AS i
               WHERE i.GalaxyID IS NOT NULL AND i.GalaxyID NOT IN
                                                (SELECT g.GalaxyID FROM lab15_1.dbo.Galaxy AS g))
        BEGIN
            RAISERROR(N'GalaxyID отсутсвтует в таблице Galaxy', 16, 2);
            ROLLBACK TRANSACTION;
        END;
END
GO

UPDATE lab15_2.dbo.Star
SET Mass = Mass + 1
WHERE StarCatalogNumber = 'NGC111'

-- UPDATE lab15_2.dbo.Star
-- SET GalaxyID = 5
-- WHERE GalaxyID = 1

SELECT * FROM lab15_2.dbo.Star
SELECT * FROM lab15_1.dbo.GalaxyStarView
GO
