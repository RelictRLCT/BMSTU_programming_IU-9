#include <iostream>
#include <chrono>
#include "GL/gl.h"
#include "GLFW/glfw3.h"
#include "cmath"
using namespace std;
int anglez = 0;
int anglex = 0;
int angley = 0;
float delta_y = 0;
bool Falling = false;
bool Jump = false;
float current_vel = 0;
bool tex = true;
bool vertex_array_optimize = false;
bool display_lists_optimize = false;

auto begin_ti = chrono::steady_clock::now();
auto begin_time = chrono::steady_clock::now();

GLuint vertexCommandsList;
GLuint cubeviewCommandsList;
GLuint displayCommandsList;

void CreateListOfVertexCommands() {
    vertexCommandsList = glGenLists(1);
    glNewList(vertexCommandsList, GL_COMPILE);

    glEnableClientState(GL_VERTEX_ARRAY);
    glEnableClientState(GL_COLOR_ARRAY);
    glEnableClientState(GL_NORMAL_ARRAY);
    glEnableClientState(GL_TEXTURE_COORD_ARRAY);

    glEndList();
}

void CreateListOfCubeView() {
    cubeviewCommandsList = glGenLists(1);
    glNewList(cubeviewCommandsList, GL_COMPILE);
    GLfloat color1[] = {1, 0, 0};
    GLfloat normal_vector1[] = {0, -1, 0};
    GLfloat color2[] = {0, 1, 0};
    GLfloat normal_vector2[] = {0, 0, -1};
    GLfloat color3[] = {0, 0, 1};
    GLfloat normal_vector3[] = {-1, 0, 0};
    GLfloat color4[] = {0, 1, 1};
    GLfloat normal_vector4[] = {0, -1, 0};
    GLfloat color5[] = {1, 0, 1};
    GLfloat normal_vector5[] = {0, 0, -1};
    GLfloat color6[] = {1, 1, 0.4};
    GLfloat normal_vector6[] = {1, 0, 0};

    glBegin(GL_POLYGON);
    // Нижняя грань
    glColor3fv(color1);
    glNormal3fv(normal_vector1);

    glTexCoord2f(1, 0);
    glVertex3f(0.3, -0.3 + delta_y, 0.3);
    glTexCoord2f(1, 1);
    glVertex3f(0.3, -0.3 + delta_y, -0.3);
    glTexCoord2f(0, 1);
    glVertex3f(-0.3, -0.3 + delta_y, -0.3);
    glTexCoord2f(0, 0);
    glVertex3f(-0.3, -0.3 + delta_y, 0.3);
    glEnd();

    // перед
    glBegin(GL_POLYGON);
    glColor3fv(color2);
    glNormal3fv(normal_vector2);

    glTexCoord2f(0, 0);
    glVertex3f(-0.3, -0.3 + delta_y, 0.3);
    glTexCoord2f(1, 0);
    glVertex3f(0.3, -0.3 + delta_y, 0.3);
    glTexCoord2f(1, 1);
    glVertex3f(0.3, 0.3 + delta_y, 0.3);
    glTexCoord2f(0, 1);
    glVertex3f(-0.3, 0.3 + delta_y, 0.3);
    glEnd();

    // Правая грань
    glBegin(GL_POLYGON);
    glColor3fv(color3);
    glNormal3fv(normal_vector3);

    glTexCoord2f(0, 0);
    glVertex3f(0.3, -0.3 + delta_y, 0.3);
    glTexCoord2f(1, 0);
    glVertex3f(0.3, -0.3 + delta_y, -0.3);
    glTexCoord2f(1, 1);
    glVertex3f(0.3, 0.3 + delta_y, -0.3);
    glTexCoord2f(0, 1);
    glVertex3f(0.3, 0.3 + delta_y, 0.3);

    glEnd();

    // Верхняя грань
    glBegin(GL_POLYGON);
    glColor3fv(color4);
    glNormal3fv(normal_vector4);

    glTexCoord2f(0, 1);
    glVertex3f(0.3, 0.3 + delta_y, 0.3);
    glTexCoord2f(0, 0);
    glVertex3f(0.3, 0.3 + delta_y, -0.3);
    glTexCoord2f(1, 0);
    glVertex3f(-0.3, 0.3 + delta_y, -0.3);
    glTexCoord2f(1, 1);
    glVertex3f(-0.3, 0.3 + delta_y, 0.3);
    glEnd();

    // Задняя грань
    glBegin(GL_POLYGON);
    glColor3fv(color5);
    glNormal3fv(normal_vector5);

    glTexCoord2f(1, 0);
    glVertex3f(0.3, -0.3 + delta_y, -0.3);
    glTexCoord2f(1, 1);
    glVertex3f(0.3, 0.3 + delta_y, -0.3);
    glTexCoord2f(0, 1);
    glVertex3f(-0.3, 0.3 + delta_y, -0.3);
    glTexCoord2f(0, 0);
    glVertex3f(-0.3, -0.3 + delta_y, -0.3);
    glEnd();

    // Левая грань
    glBegin(GL_POLYGON);
    glColor3fv(color6);
    glNormal3fv(normal_vector6);

    glTexCoord2f(1, 0);
    glVertex3f(-0.3, -0.3 + delta_y, 0.3);
    glTexCoord2f(1, 1);
    glVertex3f(-0.3, 0.3 + delta_y, 0.3);
    glTexCoord2f(0, 1);
    glVertex3f(-0.3, 0.3 + delta_y, -0.3);
    glTexCoord2f(0, 0);
    glVertex3f(-0.3, -0.3 + delta_y, -0.3);
    glEnd();
    glEndList();
}

void CreateListOfDisplayCommands() {
    displayCommandsList = glGenLists(1);
    glNewList(displayCommandsList, GL_COMPILE);

    glPushMatrix();
    glMatrixMode(GL_MODELVIEW);

    glEndList();
}

void ViewCube(){
    auto end = chrono::steady_clock::now();

    auto elapsed_ms = chrono::duration_cast<chrono::milliseconds>(end - begin_ti);
    auto full_ms = chrono::duration_cast<chrono::milliseconds>(end - begin_time);

    if (Falling and delta_y < -0.7) {
        Jump = true;
        Falling = false;
        begin_time = chrono::steady_clock::now();
        end = chrono::steady_clock::now();
        full_ms = chrono::duration_cast<chrono::milliseconds>(end - begin_time);
    }

    if (Jump and delta_y > 0) {
        Falling = true;
        Jump = false;
        begin_time = chrono::steady_clock::now();
        end = chrono::steady_clock::now();
        full_ms = chrono::duration_cast<chrono::milliseconds>(end - begin_time);
    }

    if (Falling and elapsed_ms > chrono::milliseconds(1)) {
        begin_ti = end;
        delta_y =-9.8 * pow((full_ms.count() / 1000.0), 2) / 2;
        current_vel = 9.8 * (full_ms.count() / 1000.0);
    } else if (Jump and elapsed_ms > chrono::milliseconds(10)) {
        begin_ti = end;
        delta_y = current_vel*(full_ms.count() / 1000.0) -0.7 - 9.8 * pow((full_ms.count() / 1000.0), 2) / 2;
    }


    if (vertex_array_optimize) {
        GLfloat cubeVertexArray[24][3];
        GLubyte cubeIndexArray[6][4];

        GLfloat normals[] = {
                0, 1, 0,  // Нижняя грань
                0, 1, 0,
                0, 1, 0,
                0, 1, 0,

                0, 0, -1,  // Передняя грань
                0, 0, -1,
                0, 0, -1,
                0, 0, -1,

                0, -1, 0,   // Верхняя грань
                0, -1, 0,
                0, -1, 0,
                0, -1, 0,

                0, 0, 1,   // Задняя грань
                0, 0, 1,
                0, 0, 1,
                0, 0, 1,

                1, 0, 0,  // Левая грань
                1, 0, 0,
                1, 0, 0,
                1, 0, 0,

                -1, 0, 0,    // Правая грань
                -1, 0, 0,
                -1, 0, 0,
                -1, 0, 0,

        };

        {//низ
            cubeVertexArray[0][0] = 0.3;
            cubeVertexArray[0][1] = -0.3 + delta_y;
            cubeVertexArray[0][2] = 0.3;

            cubeVertexArray[1][0] = -0.3;
            cubeVertexArray[1][1] = -0.3 + delta_y;
            cubeVertexArray[1][2] = 0.3;

            cubeVertexArray[2][0] = -0.3;
            cubeVertexArray[2][1] = -0.3 + delta_y;
            cubeVertexArray[2][2] = -0.3;

            cubeVertexArray[3][0] = 0.3;
            cubeVertexArray[3][1] = -0.3 + delta_y;
            cubeVertexArray[3][2] = -0.3;
        }

        {//перед
            cubeVertexArray[4][0] = 0.3;
            cubeVertexArray[4][1] = -0.3 + delta_y;
            cubeVertexArray[4][2] = 0.3;

            cubeVertexArray[5][0] = 0.3;
            cubeVertexArray[5][1] = 0.3 + delta_y;
            cubeVertexArray[5][2] = 0.3;

            cubeVertexArray[6][0] = -0.3;
            cubeVertexArray[6][1] = 0.3 + delta_y;
            cubeVertexArray[6][2] = 0.3;

            cubeVertexArray[7][0] = -0.3;
            cubeVertexArray[7][1] = -0.3 + delta_y;
            cubeVertexArray[7][2] = 0.3;
        }

        {//Верх
            cubeVertexArray[8][0] = 0.3;
            cubeVertexArray[8][1] = 0.3 + delta_y;
            cubeVertexArray[8][2] = 0.3;

            cubeVertexArray[9][0] = 0.3;
            cubeVertexArray[9][1] = 0.3 + delta_y;
            cubeVertexArray[9][2] = -0.3;

            cubeVertexArray[10][0] = -0.3;
            cubeVertexArray[10][1] = 0.3 + delta_y;
            cubeVertexArray[10][2] = -0.3;

            cubeVertexArray[11][0] = -0.3;
            cubeVertexArray[11][1] = 0.3 + delta_y;
            cubeVertexArray[11][2] = 0.3;
        }

        {//зад
            cubeVertexArray[12][0] = 0.3;
            cubeVertexArray[12][1] = 0.3 + delta_y;
            cubeVertexArray[12][2] = -0.3;

            cubeVertexArray[13][0] = 0.3;
            cubeVertexArray[13][1] = -0.3 + delta_y;
            cubeVertexArray[13][2] = -0.3;

            cubeVertexArray[14][0] = -0.3;
            cubeVertexArray[14][1] = -0.3 + delta_y;
            cubeVertexArray[14][2] = -0.3;

            cubeVertexArray[15][0] = -0.3;
            cubeVertexArray[15][1] = 0.3 + delta_y;
            cubeVertexArray[15][2] = -0.3;
        }

        {//лево
            cubeVertexArray[16][0] = -0.3;
            cubeVertexArray[16][1] = -0.3 + delta_y;
            cubeVertexArray[16][2] = 0.3;

            cubeVertexArray[17][0] = -0.3;
            cubeVertexArray[17][1] = 0.3 + delta_y;
            cubeVertexArray[17][2] = 0.3;

            cubeVertexArray[18][0] = -0.3;
            cubeVertexArray[18][1] = 0.3 + delta_y;
            cubeVertexArray[18][2] = -0.3;

            cubeVertexArray[19][0] = -0.3;
            cubeVertexArray[19][1] = -0.3 + delta_y;
            cubeVertexArray[19][2] = -0.3;
        }

        {//право
            cubeVertexArray[20][0] = 0.3;
            cubeVertexArray[20][1] = -0.3 + delta_y;
            cubeVertexArray[20][2] = 0.3;

            cubeVertexArray[21][0] = 0.3;
            cubeVertexArray[21][1] = -0.3 + delta_y;
            cubeVertexArray[21][2] = -0.3;

            cubeVertexArray[22][0] = 0.3;
            cubeVertexArray[22][1] = 0.3 + delta_y;
            cubeVertexArray[22][2] = -0.3;

            cubeVertexArray[23][0] = 0.3;
            cubeVertexArray[23][1] = 0.3 + delta_y;
            cubeVertexArray[23][2] = 0.3;
        }

        //низ
        cubeIndexArray[0][0] = 0;
        cubeIndexArray[0][1] = 1;
        cubeIndexArray[0][2] = 2;
        cubeIndexArray[0][3] = 3;
        //перед
        cubeIndexArray[1][0] = 4;
        cubeIndexArray[1][1] = 5;
        cubeIndexArray[1][2] = 6;
        cubeIndexArray[1][3] = 7;
        //верх
        cubeIndexArray[2][0] = 8;
        cubeIndexArray[2][1] = 9;
        cubeIndexArray[2][2] = 10;
        cubeIndexArray[2][3] = 11;
        //зад
        cubeIndexArray[3][0] = 12;
        cubeIndexArray[3][1] = 13;
        cubeIndexArray[3][2] = 14;
        cubeIndexArray[3][3] = 15;
        //лево
        cubeIndexArray[4][0] = 16;
        cubeIndexArray[4][1] = 17;
        cubeIndexArray[4][2] = 18;
        cubeIndexArray[4][3] = 19;
        //право
        cubeIndexArray[5][0] = 20;
        cubeIndexArray[5][1] = 21;
        cubeIndexArray[5][2] = 22;
        cubeIndexArray[5][3] = 23;

        GLfloat cubeTextureArray[] = {
                0, 0,
                1, 0,
                1, 1,
                0, 1,

                0, 1,
                0, 0,
                1, 0,
                1, 1,

                0, 1,
                0, 0,
                1, 0,
                1, 1,

                0, 1,
                0, 0,
                1, 0,
                1, 1,

                1, 0,
                1, 1,
                0, 1,
                0, 0,

                0, 0,
                1, 0,
                1, 1,
                0, 1,
        };

        glVertexPointer(3, GL_FLOAT, 0, cubeVertexArray);
        glNormalPointer(GL_FLOAT, 0, normals);
        glTexCoordPointer(2, GL_FLOAT, 0, cubeTextureArray);

        if (display_lists_optimize) {
            glCallList(vertexCommandsList);
        } else {
            glEnableClientState(GL_VERTEX_ARRAY);
            glEnableClientState(GL_COLOR_ARRAY);
            glEnableClientState(GL_NORMAL_ARRAY);
            glEnableClientState(GL_TEXTURE_COORD_ARRAY);
        }

        for (int i = 0; i < 6; ++i) {
            glDrawElements(GL_POLYGON, 4, GL_UNSIGNED_BYTE, &cubeIndexArray[i]);
        }

    } else {
        if (display_lists_optimize) {
            if (Falling or Jump) {
                CreateListOfCubeView();
            }
            glCallList(cubeviewCommandsList);
        } else {
        //оптимизация для использования векторных версий glColor и glNormal
            GLfloat color1[] = {1, 0, 0};
            GLfloat normal_vector1[] = {0, -1, 0};
            GLfloat color2[] = {0, 1, 0};
            GLfloat normal_vector2[] = {0, 0, -1};
            GLfloat color3[] = {0, 0, 1};
            GLfloat normal_vector3[] = {-1, 0, 0};
            GLfloat color4[] = {0, 1, 1};
            GLfloat normal_vector4[] = {0, -1, 0};
            GLfloat color5[] = {1, 0, 1};
            GLfloat normal_vector5[] = {0, 0, -1};
            GLfloat color6[] = {1, 1, 0.4};
            GLfloat normal_vector6[] = {1, 0, 0};

            glBegin(GL_POLYGON);
            // Нижняя грань
            glColor3fv(color1);
            glNormal3fv(normal_vector1);

            glTexCoord2f(1, 0);
            glVertex3f(0.3, -0.3 + delta_y, 0.3);
            glTexCoord2f(1, 1);
            glVertex3f(0.3, -0.3 + delta_y, -0.3);
            glTexCoord2f(0, 1);
            glVertex3f(-0.3, -0.3 + delta_y, -0.3);
            glTexCoord2f(0, 0);
            glVertex3f(-0.3, -0.3 + delta_y, 0.3);
            glEnd();

            // перед
            glBegin(GL_POLYGON);
            glColor3fv(color2);
            glNormal3fv(normal_vector2);

            glTexCoord2f(0, 0);
            glVertex3f(-0.3, -0.3 + delta_y, 0.3);
            glTexCoord2f(1, 0);
            glVertex3f(0.3, -0.3 + delta_y, 0.3);
            glTexCoord2f(1, 1);
            glVertex3f(0.3, 0.3 + delta_y, 0.3);
            glTexCoord2f(0, 1);
            glVertex3f(-0.3, 0.3 + delta_y, 0.3);
            glEnd();

            // Правая грань
            glBegin(GL_POLYGON);
            glColor3fv(color3);
            glNormal3fv(normal_vector3);

            glTexCoord2f(0, 0);
            glVertex3f(0.3, -0.3 + delta_y, 0.3);
            glTexCoord2f(1, 0);
            glVertex3f(0.3, -0.3 + delta_y, -0.3);
            glTexCoord2f(1, 1);
            glVertex3f(0.3, 0.3 + delta_y, -0.3);
            glTexCoord2f(0, 1);
            glVertex3f(0.3, 0.3 + delta_y, 0.3);

            glEnd();

            // Верхняя грань
            glBegin(GL_POLYGON);
            glColor3fv(color4);
            glNormal3fv(normal_vector4);

            glTexCoord2f(0, 1);
            glVertex3f(0.3, 0.3 + delta_y, 0.3);
            glTexCoord2f(0, 0);
            glVertex3f(0.3, 0.3 + delta_y, -0.3);
            glTexCoord2f(1, 0);
            glVertex3f(-0.3, 0.3 + delta_y, -0.3);
            glTexCoord2f(1, 1);
            glVertex3f(-0.3, 0.3 + delta_y, 0.3);
            glEnd();

            // Задняя грань
            glBegin(GL_POLYGON);
            glColor3fv(color5);
            glNormal3fv(normal_vector5);

            glTexCoord2f(1, 0);
            glVertex3f(0.3, -0.3 + delta_y, -0.3);
            glTexCoord2f(1, 1);
            glVertex3f(0.3, 0.3 + delta_y, -0.3);
            glTexCoord2f(0, 1);
            glVertex3f(-0.3, 0.3 + delta_y, -0.3);
            glTexCoord2f(0, 0);
            glVertex3f(-0.3, -0.3 + delta_y, -0.3);
            glEnd();

            // Левая грань
            glBegin(GL_POLYGON);
            glColor3fv(color6);
            glNormal3fv(normal_vector6);

            glTexCoord2f(1, 0);
            glVertex3f(-0.3, -0.3 + delta_y, 0.3);
            glTexCoord2f(1, 1);
            glVertex3f(-0.3, 0.3 + delta_y, 0.3);
            glTexCoord2f(0, 1);
            glVertex3f(-0.3, 0.3 + delta_y, -0.3);
            glTexCoord2f(0, 0);
            glVertex3f(-0.3, -0.3 + delta_y, -0.3);
            glEnd();
        }
    }
}

void enable_light() {
    //GLfloat light0_diffuse[] = {0.6, 0.7, 0.2};
    GLfloat light0_diffuse[] = {1.0, 1.0, 1.0};
    GLfloat light0_direction[] = {-0.5, 0, -1, 1.0};
    glEnable(GL_LIGHT0);
    glLightfv(GL_LIGHT0, GL_DIFFUSE, light0_diffuse);
    glLightfv(GL_LIGHT0, GL_POSITION, light0_direction);
}


GLuint loadBMP(const char * imagepath) {
    unsigned char header[54];
    unsigned int dataPos;
    unsigned int width, height;
    unsigned int imageSize;
    unsigned char * data;

    FILE * file = fopen(imagepath,"rb");
    if (!file) {
        cout << "Файл не найден" << endl;
        return 0;
    }

    if ( fread(header, 1, 54, file) != 54 ) {
        cout << "Некорректный BMP-файлn" << endl;
        return false;
    }

    if ( header[0]!='B' || header[1]!='M' ){
        cout << "Некорректный BMP-файлn" << endl;
        return 0;
    }

    dataPos    = *(int*)&(header[0x0A]); // Смещение данных изображения в файле
    imageSize  = *(int*)&(header[0x22]); // Размер изображения в байтах
    width      = *(int*)&(header[0x12]); // Ширина
    height     = *(int*)&(header[0x16]); // Высота

    if (imageSize==0)    imageSize=width*height*3;
    if (dataPos==0)      dataPos=54;

    data = new unsigned char [imageSize];

    fread(data,1,imageSize,file);

    fclose(file);

    GLuint textureID;
    glGenTextures(1, &textureID);

    glBindTexture(GL_TEXTURE_2D, textureID);

    glTexImage2D(GL_TEXTURE_2D, 0,GL_RGB, width, height, 0, GL_BGR, GL_UNSIGNED_BYTE, data);

    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
    return textureID;
}


void display(GLFWwindow *window) {
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
    glLoadIdentity();
    glEnable(GL_DEPTH_TEST);

    GLfloat material_diffuse[] = {0.4, 0.6, 0.3, 1.0};
    //glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, material_diffuse);
    glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, material_diffuse);

    glClearColor(0.1, 0.2, 0.4, 0.0);


    GLfloat rotateCubeX[] = {
            1.0f, 0.0f, 0.0f, 0.0f,
            0.0f, static_cast<GLfloat>(cos(anglex * M_PI / 180)), -static_cast<GLfloat>(sin(anglex * M_PI / 180)), 0.0f,
            0.0f, static_cast<GLfloat>(sin(anglex * M_PI / 180)), static_cast<GLfloat>(cos(anglex * M_PI / 180)), 0.0f,
            0.0f, 0.0f, 0.0f, 1.0f
    };

    GLfloat rotateCubeY[] = {
            static_cast<GLfloat>(cos(angley * M_PI / 180)), 0.0f, static_cast<GLfloat>(sin(angley * M_PI / 180)), 0.0f,
            0.0f, 1.0f, 0.0f, 0.0f,
            -static_cast<GLfloat>(sin(angley * M_PI / 180)), 0.0f, static_cast<GLfloat>(cos(angley * M_PI / 180)), 0.0f,
            0.0f, 0.0f, 0.0f, 1.0f
    };

    GLfloat rotateCubeZ[] = {
            static_cast<GLfloat>(cos(anglez * M_PI / 180)), -static_cast<GLfloat>(sin(anglez * M_PI / 180)), 0.0f, 0.0f,
            static_cast<GLfloat>(sin(anglez * M_PI / 180)), static_cast<GLfloat>(cos(anglez * M_PI / 180)), 0.0f, 0.0f,
            0.0f, 0.0f, 1.0f, 0.0f,
            0.0f, 0.0f, 0.0f, 1.0f
    };

    if (display_lists_optimize) {
        glCallList(displayCommandsList);
    } else {
        glPushMatrix();
        glMatrixMode(GL_MODELVIEW);
    }

    glMultMatrixf(rotateCubeX);
    glMultMatrixf(rotateCubeY);
    glMultMatrixf(rotateCubeZ);

    ViewCube();

    glPopMatrix();
    enable_light();
    glFlush();
    glfwSwapBuffers(window);
    glfwPollEvents();
}


void CallbackKeys(GLFWwindow* window, int key, int scancode, int action, int mods) {

    if (action == GLFW_PRESS) {
        switch (key) {
            case GLFW_KEY_RIGHT:
                angley += 6;
                break;

            case GLFW_KEY_LEFT:
                angley -= 6;
                break;

            case GLFW_KEY_UP:
                anglex -= 6;
                break;

            case GLFW_KEY_DOWN:
                anglex += 6;
                break;

            case GLFW_KEY_LEFT_CONTROL:
                if (Falling) {
                    Falling = !Falling;
                    Jump = false;
                    delta_y = 0;
                } else if (Jump) {
                    Jump = false;
                    delta_y = 0;
                } else {
                    Falling = !Falling;
                }
                CreateListOfCubeView();
                begin_ti = chrono::steady_clock::now();
                begin_time = chrono::steady_clock::now();
                break;
            case GLFW_KEY_LEFT_ALT:
                anglez = 0;
                anglex = 0;
                angley = 0;
                delta_y = 0;
                Falling = false;
                Jump = false;
                current_vel = 0;
                CreateListOfCubeView();
                break;
            case GLFW_KEY_T:
                if (tex) {
                    glDisable(GL_TEXTURE_2D);
                } else {
                    glEnable(GL_TEXTURE_2D);
                }
                tex = !tex;
                break;
            case GLFW_KEY_M:
                vertex_array_optimize = !vertex_array_optimize;
                break;
            case GLFW_KEY_D:
                display_lists_optimize = !display_lists_optimize;
                break;
        }
    }
}

void CallbackScroll(GLFWwindow* window, double xoffset, double yoffset) {

    if (yoffset > 0) {
        anglez += 3;
    } else {
        anglez -=3;
    }
}

int main() {

    cout << "УПРАВЛЕНИЕ ОБАМОЙ " << endl << "Вращение: стрелки и колесо мыши" << endl
    << "Левый Ctrl: заставить Обаму прыгать или остановиться" << endl << "Левый Alt: отменить все действия" << endl
    << "T: вкл./выкл. текстуру" << endl << "M: вкл./выкл. оптимизацию с массивами вершин" << endl
    << "D: вкл./выкл. оптимизацию с дисплейными списками" << endl;

    glfwInit();
    GLFWwindow *window;

    window = glfwCreateWindow(840, 840, "Laba 7", NULL, NULL);
    glfwMakeContextCurrent(window);

    glEnable(GL_LIGHTING);
    glLightModelf(GL_LIGHT_MODEL_TWO_SIDE, GL_TRUE);
    glDisable(GL_NORMALIZE);

    GLuint texture = loadBMP("../obama.bmp");
    glEnable(GL_TEXTURE_2D);
    glBindTexture(GL_TEXTURE_2D,texture);
    glTexEnvf(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE,GL_MODULATE);

    glfwSetKeyCallback(window, CallbackKeys);
    glfwSetScrollCallback(window, CallbackScroll);

    CreateListOfVertexCommands();//создание дисплейного списка для отрисовки куба с массивом вершин
    CreateListOfCubeView();//создание дисплейного списка для отрисовки куба без массива вершин
    CreateListOfDisplayCommands();//создание дисплейного списка для запоминания нескольких команд в display()

    while (not glfwWindowShouldClose(window)) {
        display(window);
    }

    glfwDestroyWindow(window);
    glfwTerminate();
    return 0;
}
