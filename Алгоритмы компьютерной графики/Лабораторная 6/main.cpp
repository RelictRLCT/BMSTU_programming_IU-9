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

auto begin_ti = chrono::steady_clock::now();
auto begin_time = chrono::steady_clock::now();

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


    glBegin(GL_POLYGON);
    // Нижняя грань
    glColor3f(1, 0, 0);
    glNormal3f(0, -1, 0);

    glTexCoord2f( 1, 0 );
    glVertex3f(0.3, -0.3 + delta_y, 0.3);
    glTexCoord2f( 1, 1 );
    glVertex3f(0.3, -0.3 + delta_y, -0.3);
    glTexCoord2f( 0, 1 );
    glVertex3f(-0.3, -0.3 + delta_y, -0.3);
    glTexCoord2f( 0, 0 );
    glVertex3f(-0.3, -0.3 + delta_y, 0.3);
    glEnd();

    // перед
    glBegin(GL_POLYGON);
    glColor3f(0, 1, 0);
    glNormal3f(0, 0, -1);
    glTexCoord2f( 0, 0 );
    glVertex3f(-0.3, -0.3 + delta_y, 0.3);
    glTexCoord2f( 1, 0 );
    glVertex3f(0.3, -0.3 + delta_y, 0.3);
    glTexCoord2f( 1, 1 );
    glVertex3f(0.3, 0.3 + delta_y, 0.3);
    glTexCoord2f(0, 1);
    glVertex3f(-0.3, 0.3 + delta_y, 0.3);
    glEnd();

    // Правая грань
    glBegin(GL_POLYGON);
    glColor3f(0, 0, 1);
    glNormal3f(-1, 0, 0);

    glTexCoord2f( 0, 0 );
    glVertex3f(0.3, -0.3 + delta_y, 0.3);
    glTexCoord2f( 1, 0 );
    glVertex3f(0.3, -0.3 + delta_y, -0.3);
    glTexCoord2f( 1, 1 );
    glVertex3f(0.3, 0.3 + delta_y, -0.3);
    glTexCoord2f( 0, 1 );
    glVertex3f(0.3, 0.3 + delta_y, 0.3);

    glEnd();

    // Верхняя грань
    glBegin(GL_POLYGON);
    glColor3f(0, 1, 1);
    glNormal3f(0, -1, 0);

    glTexCoord2f( 0, 1 );
    glVertex3f(0.3, 0.3 + delta_y, 0.3);
    glTexCoord2f( 0, 0 );
    glVertex3f(0.3, 0.3 + delta_y, -0.3);
    glTexCoord2f( 1, 0 );
    glVertex3f(-0.3, 0.3 + delta_y, -0.3);
    glTexCoord2f( 1, 1 );
    glVertex3f(-0.3, 0.3 + delta_y, 0.3);
    glEnd();

    // Задняя грань
    glBegin(GL_POLYGON);
    glColor3f(1, 0, 1);
    glNormal3f(0, 0, -1);

    glTexCoord2f( 1, 0 );
    glVertex3f(0.3, -0.3 + delta_y, -0.3);
    glTexCoord2f( 1, 1 );
    glVertex3f(0.3, 0.3 + delta_y, -0.3);
    glTexCoord2f( 0, 1 );
    glVertex3f(-0.3, 0.3 + delta_y, -0.3);
    glTexCoord2f( 0, 0 );
    glVertex3f(-0.3, -0.3 + delta_y, -0.3);
    glEnd();

    // Левая грань
    glBegin(GL_POLYGON);
    glColor3f(1, 1, 0.4);
    glNormal3f(1, 0, 0);

    glTexCoord2f( 1, 0 );
    glVertex3f(-0.3, -0.3 + delta_y, 0.3);
    glTexCoord2f( 1, 1 );
    glVertex3f(-0.3, 0.3 + delta_y, 0.3);
    glTexCoord2f( 0, 1 );
    glVertex3f(-0.3, 0.3 + delta_y, -0.3);
    glTexCoord2f( 0, 0 );
    glVertex3f(-0.3, -0.3 + delta_y, -0.3);
    glEnd();
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

    glPushMatrix();
    glMatrixMode(GL_MODELVIEW);

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
                break;
            case GLFW_KEY_T:
                if (tex) {
                    glDisable(GL_TEXTURE_2D);
                } else {
                    glEnable(GL_TEXTURE_2D);
                }
                tex = !tex;
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

    glfwInit();
    GLFWwindow *window;

    window = glfwCreateWindow(840, 840, "Laba 6", NULL, NULL);
    glfwMakeContextCurrent(window);

    glEnable(GL_LIGHTING);
    glLightModelf(GL_LIGHT_MODEL_TWO_SIDE, GL_TRUE);
    glEnable(GL_NORMALIZE);

    GLuint texture = loadBMP("../obama.bmp");
    glEnable(GL_TEXTURE_2D);
    glBindTexture(GL_TEXTURE_2D,texture);
    glTexEnvf(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE,GL_MODULATE);

    glfwSetKeyCallback(window, CallbackKeys);
    glfwSetScrollCallback(window, CallbackScroll);

    while (not glfwWindowShouldClose(window)) {
        display(window);
    }

    glfwDestroyWindow(window);
    glfwTerminate();
    return 0;
}
